namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Object.Diff
open FSharp.Object.Diff.Dictionary

type private Translator(subPrefix: string, addPrefix: string) =

  let prefix p (o: obj) = String.indent 1 p + " " + String.toSingleLineString o
  let sub (o: obj) = prefix subPrefix o
  let add (o: obj) = prefix addPrefix o

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

  let translateChange (node: DiffNode) (base_: obj) (modified: obj) =
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
          node.Path.ToString()
          sub base_
          add modified
        ]
      else
        translateUnion node base_ modified

  member __.Translate(node: DiffNode, base_: obj, modified: obj) =
    match node.State with
    | Changed -> translateChange node base_ modified
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

type CustomAssertionVisitor =
  inherit NodeVisitor
  abstract member Diff: string

[<Sealed>]
type internal AssertionVisitor(subPrefix: string, addPrefix: string, working: obj, base_: obj) =

  let diff = ResizeArray<string>()
  let translator = Translator(subPrefix, addPrefix)

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    let ds =
      translator.Translate(node, node.CanonicalGet(base_), node.CanonicalGet(modified))
    diff.AddRange(ds)

  member __.Diff =
    diff
    |> String.concat Environment.NewLine

  interface CustomAssertionVisitor with
    member this.Diff = this.Diff

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
