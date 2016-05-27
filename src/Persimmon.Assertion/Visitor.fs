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

module private Path =

  open System.Text
  open Printf

  let toStr (path: NodePath) =
    let builder = StringBuilder()
    let rec loop (previous: ElementSelector option) (selectors: ElementSelector list) =
      match previous, selectors with
      | _, [] -> ()
      | _, x :: xs when (x :? RootElementSelector) ->
        builder.Append(".") |> ignore
        loop (Some x) xs
      | Some p, x :: xs when (p :? RootElementSelector) ->
        bprintf builder "%O" x
        loop (Some x) xs
      | _, x :: xs ->
        bprintf builder ".%O" x
        loop (Some x) xs
    loop None path.ElementSelectors
    builder.ToString()

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
        Path.toStr node.ParentNode.Path
        base_ |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> sub
        modified |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> add
      ]
    else
      [
        Path.toStr node.Path
        sub base_
        add modified
      ]

  let translateChange (node: DiffNode) (base_: obj) (modified: obj) =
    match base_, modified with
    | null, null -> []
    | null, _ ->
      [
        Path.toStr node.Path
        add modified
      ]
    | _, null ->
      [
        Path.toStr node.Path
        sub base_
      ]
    | _ ->
      if node.IsRootNode ||  not <| FSharpType.IsUnion(node.ParentNode.Type) then
        [
          Path.toStr node.Path
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
        Path.toStr node.Path
        add modified
      ]
    | Removed ->
      [
        Path.toStr node.Path
        sub base_
      ]
    | _ -> []

  member __.Add(node: DiffNode, base_: obj, modified: obj) =
    match node.State with
    | Changed | Added | Removed -> diff.Add({ Node = node; Base = base_; Modified = modified })
    | _ -> ()

  member __.Translate() =
    diff
    |> Seq.groupBy (fun x -> Path.toStr x.Node.Path)
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
