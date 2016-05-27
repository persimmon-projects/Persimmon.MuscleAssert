namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Object.Diff
open FSharp.Object.Diff.Dictionary

type private Difference = {
  Node: DiffNode
  Expected: obj
  Actual: obj
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

type private Translator(expectedPrefix: string, actualPrefix: string) =

  let diff = ResizeArray<Difference>()

  let fixedExpectedPrefix, fixedActualPrefix =
    let el = String.length expectedPrefix
    let al = String.length actualPrefix
    let pad = String.replicate (abs (el - al)) " "
    let e =if el < al then expectedPrefix + pad else expectedPrefix
    let a = if el > al then actualPrefix + pad else actualPrefix
    (e, a)

  let prefix p (o: obj) = String.indent 1 p + " " + String.toSingleLineString o
  let appendExpectedPrefix (o: obj) = prefix fixedExpectedPrefix o
  let appendActualPrefix (o: obj) = prefix fixedActualPrefix o
  let appendOnlyExpectedPrefix (o: obj) = prefix expectedPrefix o
  let appendOnlyActualPrefix (o: obj) = prefix actualPrefix o

  let unionTag (cases: UnionCaseInfo []) typ tag =
    let info = cases |> Array.find (fun x -> x.Tag = tag)
    info.Name

  let translateUnion (node: DiffNode) expected actual =
    let typ = node.ParentNode.Type
    let cases = FSharpType.GetUnionCases(typ)
    if cases |> Array.exists (fun x -> node.PropertyName = "Is" + x.Name) then []
    elif node.PropertyName = "Tag" then
      [
        Path.toStr node.ParentNode.Path
        expected |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> appendExpectedPrefix
        actual |> unbox<int> |> unionTag cases typ |> sprintf "%s.%s" typ.Name |> appendActualPrefix
      ]
    else
      [
        Path.toStr node.Path
        appendExpectedPrefix expected
        appendActualPrefix actual
      ]

  let translateChange (node: DiffNode) (expected: obj) (actual: obj) =
    match expected, actual with
    | null, null -> []
    | null, _ ->
      [
        Path.toStr node.Path
        appendOnlyActualPrefix actual
      ]
    | _, null ->
      [
        Path.toStr node.Path
        appendOnlyExpectedPrefix expected
      ]
    | _ ->
      if node.IsRootNode ||  not <| FSharpType.IsUnion(node.ParentNode.Type) then
        [
          Path.toStr node.Path
          appendExpectedPrefix expected
          appendActualPrefix actual
        ]
      else
        translateUnion node expected actual

  let translate (node: DiffNode) (expected: obj) (actal: obj) =
    match node.State with
    | Changed -> translateChange node expected actal
    | Added ->
      [
        Path.toStr node.Path
        appendOnlyActualPrefix actal
      ]
    | Removed ->
      [
        Path.toStr node.Path
        appendOnlyExpectedPrefix expected
      ]
    | _ -> []

  member __.Add(node: DiffNode, expected: obj, actual: obj) =
    match node.State with
    | Changed | Added | Removed -> diff.Add({ Node = node; Expected = expected; Actual = actual })
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
          if x.Actual = y.Expected then []
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            [{ x with Expected = y.Expected }]
        | (Removed, Added) ->
          if x.Expected = y.Actual then []
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            [{ x with Actual = y.Actual }]
        | (Changed, Changed) ->
          if x.Expected = y.Actual && x.Actual = y.Expected then []
          else
            let b = if x.Expected <> null then x.Expected else y.Expected
            let m = if x.Actual <> null then x.Actual else y.Actual
            [
              {
                Node = x.Node
                Expected = b
                Actual = m
              }
            ]
        | _ -> [x; y]
      | _ -> []
    )
    |> Seq.collect (fun x -> translate x.Node x.Expected x.Actual)
    |> String.concat Environment.NewLine

type CustomAssertionVisitor =
  inherit NodeVisitor
  abstract member Diff: string

[<Sealed>]
type internal AssertionVisitor(expectedPrefix: string, expected: obj, actualPrefix: string, actual: obj) =

  let translator = Translator(expectedPrefix, actualPrefix)

  let filter (node: DiffNode) =
    (node.IsRootNode && not node.HasChanges) || (node.HasChanges && not node.HasChildren)

  member __.Diff = translator.Translate()

  interface CustomAssertionVisitor with
    member this.Diff = this.Diff

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then translator.Add(node, node.CanonicalGet(expected), node.CanonicalGet(actual))
