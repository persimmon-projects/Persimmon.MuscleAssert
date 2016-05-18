namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Object.Diff
open FSharp.Object.Diff.Dictionary

type private IndexedEnumerator = {
  Index: int
  Enumerator: IEnumerator
}

module private Translate =

  let sub (o: obj) = String.indent 1 "- " + String.toSingleLineString o
  let add (o: obj) = String.indent 1 "+ " + String.toSingleLineString o

  let unionTag (cases: UnionCaseInfo []) typ tag =
    let info = cases |> Array.find (fun x -> x.Tag = tag)
    info.Name

  let translateUnion (node: DiffNode) base_ modified =
    let typ = node.ParentNode.Type
    let cases = FSharpType.GetUnionCases(typ)
    if cases |> Array.exists (fun x -> node.PropertyName = "Is" + x.Name) then []
    elif node.PropertyName = "Tag" then
      [
        node.ParentNode.Path.ToString()
        base_ |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> sub
        modified |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> add
      ]
    else
      [
        node.Path.ToString()
        sub base_
        add modified
      ]

  let change (node: DiffNode) (base_: obj) (modified: obj) =
    match base_, modified with
    | null, null -> []
    | null, _ ->
      [
        node.Path.ToString()
        add modified
      ]
    | _, null ->
      [
        node.Path.ToString()
        sub base_
      ]
    | _ ->
      if node.IsRootNode ||  not <| FSharpType.IsUnion(node.ParentNode.Type) then
        [
          yield node.Path.ToString()
          yield sub base_
          if modified <> null then
            yield add modified
        ]
      else
        translateUnion node base_ modified

  let translateIEnumerable (hits: Dictionary<_, _>) (node: DiffNode) (base_: obj) (modified: obj) bx mx =

    let dumpPath i = sprintf "%O[%d]" node.ParentNode.Path i

    let rec inner index o (e: IEnumerator) =
      if e.MoveNext() then
        let index = index + 1
        if e.Current = o then { Index = index; Enumerator = e }
        else inner index o e
      else { Index = index; Enumerator = e }

    match node.State with
    | Changed ->
      match base_, modified with
      | null, null -> []
      | _, null ->
        let bx = inner bx.Index base_ bx.Enumerator
        hits.Remove(node) |> ignore
        hits.Add(node, (bx, mx))
        [
          dumpPath bx.Index
          sub base_
        ]
      | null, __ ->
        let mx = inner mx.Index modified mx.Enumerator
        hits.Remove(node) |> ignore
        hits.Add(node, (bx, mx))
        [
          dumpPath mx.Index
          add modified
        ]
      | _ ->
        let mx = inner bx.Index base_ bx.Enumerator
        let mx = inner mx.Index modified mx.Enumerator
        hits.Remove(node) |> ignore
        hits.Add(node, (bx, mx))
        [
          dumpPath bx.Index
          sub base_
          add modified
        ]
    | Added ->
      let mx = inner mx.Index modified mx.Enumerator
      hits.Remove(node) |> ignore
      hits.Add(node, (bx, mx))
      [
        dumpPath mx.Index
        add modified
      ]
    | Removed ->
      let bx = inner bx.Index base_ bx.Enumerator
      hits.Remove(node) |> ignore
      hits.Add(node, (bx, mx))
      [
        dumpPath bx.Index
        sub base_
      ]
    | _ -> []

  let translate (hits: Dictionary<DiffNode, IndexedEnumerator * IndexedEnumerator>) (node: DiffNode) (base_: obj) (modified: obj) =
    match hits.TryGetValue(node) with
    | true, (bx, mx) -> translateIEnumerable hits node base_ modified bx mx
    | false, _ ->
      match node.State with
      | Changed -> change node base_ modified
      | Added ->
        [
          node.Path.ToString()
          add modified
        ]
      | Removed ->
        [
          node.Path.ToString()
          sub base_
        ]
      | _ -> []

[<Sealed>]
type internal AssertionVisitor(working: obj, base_: obj) =

  let diff = ResizeArray<string>()

  let hits = Dictionary<DiffNode, IndexedEnumerator * IndexedEnumerator>()

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    let ds =
      if not <| node.IsRootNode then
        match node.ParentNode.CanonicalGet(base_) with
        | Dictionary _ -> ()
        | :? IEnumerable as b when not <| hits.ContainsKey(node) ->
          match node.ParentNode.CanonicalGet(modified) with
          | :? IEnumerable as m ->
            let b = { Index = -1; Enumerator = b.GetEnumerator() }
            let m = { Index = -1; Enumerator = m.GetEnumerator() }
            hits.Add(node, (b, m))
          | _ -> ()
        | _ -> ()
      Translate.translate hits node (node.CanonicalGet(base_)) (node.CanonicalGet(modified))
    diff.AddRange(ds)

  member __.Diff =
    diff
    |> String.concat Environment.NewLine

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
