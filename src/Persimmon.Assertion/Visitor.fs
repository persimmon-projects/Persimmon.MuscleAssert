namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Object.Diff
open FSharp.Object.Diff.Dictionary

type private Difference = {
  Node: DiffNode
  Base: obj
  Modified: obj
}

type private Translator(subPrefix: string, addPrefix: string) =

  let diff = ResizeArray<Difference>()

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

  let translate (node: DiffNode) (base_: obj) (modified: obj) =
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

  member __.Add(node: DiffNode, base_: obj, modified: obj) =
    match node.State with
    | Changed | Added | Removed -> diff.Add({ Node = node; Base = base_; Modified = modified })
    | _ -> ()

  member __.Translate() =
    diff
    |> Seq.groupBy (fun x -> x.Node.Path.ToString())
    |> Seq.collect (fun (_, ds) ->
      match List.ofSeq ds with
      | [x] -> [x]
      | [x; y] ->
        match (x.Node.State, y.Node.State) with
        | (Added, Removed) ->
          if x.Modified = y.Base then []
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            [{ x with Base = y.Base }]
        | (Removed, Added) ->
          if x.Base = y.Modified then []
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            [{ x with Modified = y.Modified }]
        | (Changed, Changed) ->
          let b = if x.Base <> null then x.Base else y.Base
          let m = if x.Modified <> null then x.Modified else y.Modified
          [
            {
              Node = x.Node
              Base = b
              Modified = m
            }
          ]
        | _ -> [x; y]
      | _ -> []
    )
    |> Seq.collect (fun x -> translate x.Node x.Base x.Modified)
    |> String.concat Environment.NewLine

type CustomAssertionVisitor =
  inherit NodeVisitor
  abstract member Diff: string

[<Sealed>]
type internal AssertionVisitor(subPrefix: string, addPrefix: string, working: obj, base_: obj) =

  let translator = Translator(subPrefix, addPrefix)

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  let dumpDiff (node: DiffNode) (base_: obj) (modified: obj) =
    translator.Add(node, node.CanonicalGet(base_), node.CanonicalGet(modified))

  member __.Diff = translator.Translate()

  interface CustomAssertionVisitor with
    member this.Diff = this.Diff

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then dumpDiff node base_ working
