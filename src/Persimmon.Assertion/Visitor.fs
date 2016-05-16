namespace Persimmon

open System
open FSharp.Object.Diff

[<Sealed>]
type internal AssertionVisitor(working: obj, base_: obj) =

  let diff = ResizeArray<string>()

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let translate (node: DiffNode) base_ modified = function
  | Ignored -> []
  | Changed ->
    [
      node.Path.ToString()
      String.indent 1 "- " + String.toSingleLineString base_
      String.indent 1 "+ " + String.toSingleLineString modified
    ]
  | Added ->
    [
      node.Path.ToString()
      String.indent 1 "+ " + String.toSingleLineString modified
    ]
  | Removed ->
    [
      node.Path.ToString()
      String.indent 1 "- " + String.toSingleLineString base_
    ]
  | _ -> []

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    let ds = translate node (node.CanonicalGet(base_)) (node.CanonicalGet(modified)) node.State
    diff.AddRange(ds)

  member __.Diff =
    diff
    |> String.concat Environment.NewLine

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
