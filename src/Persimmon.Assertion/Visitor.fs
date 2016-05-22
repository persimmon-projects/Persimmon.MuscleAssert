namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Object.Diff
open FSharp.Object.Diff.Dictionary

type private IndexedMap = Dictionary<DiffNode, Dictionary<obj, int>>

module private Path =

  let toStr (d: IndexedMap) node value =
    let rec inner acc (node: DiffNode) value =
      if node.IsRootNode then
        match acc with
        | [] -> ["/"]
        | _ -> "" :: acc
      else
        match d.TryGetValue(node.ParentNode) with
        | true, d ->
          let index = d.[node.CanonicalGet(value)]
          inner (sprintf "[%d]" index :: acc) node.ParentNode value
        | false, _ ->
          inner (node.ElementSelector.HumanReadableString :: acc) node.ParentNode value
    inner [] node value
    |> String.concat "/"

type private Translator(originalBase: obj, originalModified: obj, baseIndex: IndexedMap, modifiedIndex: IndexedMap) =

  let sub (o: obj) = String.indent 1 "- " + String.toSingleLineString o
  let add (o: obj) = String.indent 1 "+ " + String.toSingleLineString o

  let unionTag (cases: UnionCaseInfo []) typ tag =
    let info = cases |> Array.find (fun x -> x.Tag = tag)
    info.Name

  let translateUnion (index: IndexedMap) (node: DiffNode) base_ modified =
    let typ = node.ParentNode.Type
    let cases = FSharpType.GetUnionCases(typ)
    if cases |> Array.exists (fun x -> node.PropertyName = "Is" + x.Name) then []
    elif node.PropertyName = "Tag" then
      [
        Path.toStr index node.ParentNode originalBase
        base_ |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> sub
        modified |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> add
      ]
    else
      [
        Path.toStr index node originalBase
        sub base_
        add modified
      ]

  let translateChange (node: DiffNode) (base_: obj) (modified: obj) =
    match base_, modified with
    | null, null -> []
    | null, _ ->
      [
        Path.toStr modifiedIndex node originalModified
        add modified
      ]
    | _, null ->
      [
        Path.toStr baseIndex node originalBase
        sub base_
      ]
    | _ ->
      if node.IsRootNode ||  not <| FSharpType.IsUnion(node.ParentNode.Type) then
        [
          yield Path.toStr baseIndex node originalBase
          yield sub base_
          if modified <> null then
            yield add modified
        ]
      else
        translateUnion baseIndex node base_ modified

  member __.Translate(node: DiffNode, base_: obj, modified: obj) =
    match node.State with
    | Changed -> translateChange node base_ modified
    | Added ->
      [
        Path.toStr modifiedIndex node originalModified
        add modified
      ]
    | Removed ->
      [
        Path.toStr baseIndex node originalBase
        sub base_
      ]
    | _ -> []

[<Sealed>]
type internal AssertionVisitor(working: obj, base_: obj) =

  let diff = ResizeArray<string>()

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let indexed (d: Dictionary<obj, int>) (e: IEnumerable) =
    let rec inner index (e: IEnumerator) =
      if e.MoveNext() then
        let index = index + 1
        if not <| d.ContainsKey(e.Current) then d.Add(e.Current, index)
        inner index e
      else ()
    e.GetEnumerator() |> inner -1

  let collectIndex node value =
    let acc = IndexedMap()
    let rec inner (node: DiffNode) =
      if node.IsRootNode then acc
      else
        let node = node.ParentNode
        match node.CanonicalGet(value) with
        | Dictionary _ -> inner node
        | :? IEnumerable as xs ->
          match acc.TryGetValue(node) with
          | true, d ->
            indexed d xs
          | false, _ ->
            let d = Dictionary<obj, int>()
            indexed d xs
            acc.Add(node, d)
          inner node
        | _ -> inner node
    inner node

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    let ds =
      let baseIndex = collectIndex node base_
      let modifiedIndex = collectIndex node modified
      Translator(base_, modified, baseIndex, modifiedIndex).Translate(node, node.CanonicalGet(base_), node.CanonicalGet(modified))
    diff.AddRange(ds)

  member __.Diff =
    diff
    |> String.concat Environment.NewLine

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
