namespace Persimmon

open System
open FSharp.Reflection
open FSharp.Object.Diff

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

  let translate (node: DiffNode) (base_: obj) (modified: obj) = function
  | Ignored -> []
  | Changed ->
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

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    let ds = Translate.translate node (node.CanonicalGet(base_)) (node.CanonicalGet(modified)) node.State
    diff.AddRange(ds)

  member __.Diff =
    diff
    |> String.concat Environment.NewLine

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
