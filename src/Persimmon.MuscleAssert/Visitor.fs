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

  [<Literal>]
  let NestedTupleArity = 8

  let toStr (path: NodePath) =
    let builder = StringBuilder()
    let rec loop arity (previous: ElementSelector option) (selectors: ElementSelector list) =
      match previous, selectors with
      | Some(:? TupleItemElementSelector), [] ->
        bprintf builder ".%d" arity
      | Some(:? RootElementSelector), [] ->
        bprintf builder "."
      | Some(:? UnionCaseItemElementSelector as x), [] when String.IsNullOrEmpty x.HumanReadableString ->
        bprintf builder "."
      | _, [] -> ()
      | Some(:? RootElementSelector), x::xs ->
        loop arity None (x :: xs)
      | None, (:? RootElementSelector as p)::xs ->
        loop arity (Some (p :> ElementSelector)) xs
      | Some(:? TupleItemElementSelector), (:? TupleItemElementSelector as x) :: xs ->
        loop (x.Arity + arity - if arity >= NestedTupleArity then 1 else 0) (Some(x :> ElementSelector)) xs
      | Some(:? TupleItemElementSelector as p), x :: xs ->
        bprintf builder ".%d" arity
        loop 0 None (x :: xs)
      | _, (:? TupleItemElementSelector as x) :: xs ->
        loop x.Arity (Some(x :> ElementSelector)) xs
      | _, (:? UnionCaseItemElementSelector as x) :: xs ->
        if String.IsNullOrEmpty x.HumanReadableString then ()
        else bprintf builder ".%s" x.HumanReadableString
        loop arity (Some(x :> ElementSelector)) xs
      | _, x :: xs ->
        bprintf builder ".%O" x
        loop arity (Some x) xs
    loop 0 None path.ElementSelectors
    builder.ToString()

type TranslateResult = {
  Diff: string seq
  Ignored: string seq
}

type private Translator(expectedPrefix: string, actualPrefix: string) =

  let diffs = ResizeArray<Difference>()
  let ignored = ResizeArray<DiffNode>()
  
  let maxShowableSeqProperties = 10

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
    [
      if node.PropertyName = DU.Tag then node.ParentNode.Path else node.Path
      |> Path.toStr
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

  let translate (node: DiffNode) (expected: obj) (actual: obj) =
    match node.State with
    | Changed -> translateChange node expected actual
    | Added ->
      [
        Path.toStr node.Path
        appendOnlyActualPrefix actual
      ]
    | Removed ->
      [
        Path.toStr node.Path
        appendOnlyExpectedPrefix expected
      ]
    | _ -> []

  member __.Add(node: DiffNode, expected: obj, actual: obj) =
    match node.State with
    | Changed | Added | Removed -> diffs.Add({ Node = node; Expected = expected; Actual = actual })
    | Ignored -> ignored.Add(node)
    | _ -> ()

  member __.Translate() =
    let ignored =
      let paths =
        ignored
        |> Seq.map (fun x -> Path.toStr x.Path |> String.indent 1)
        |> Seq.distinct
      if Seq.isEmpty paths then paths
      else
        seq {
          yield "seq<'T> or System.Collection.IEnumerable properties do not verify."
          yield "Please apply Seq.toList and assert."
          if Seq.length paths > maxShowableSeqProperties then
            yield! Seq.take maxShowableSeqProperties paths
            yield
              paths
              |> Seq.skip maxShowableSeqProperties
              |> Seq.length
              |> sprintf "and more %d properties."
              |> String.indent 1
          else
            yield! paths
        }
    diffs
    |> Seq.groupBy (fun x -> Path.toStr x.Node.Path)
    |> Seq.collect (fun (_, ds) ->
      match List.ofSeq ds with
      | [x] -> Seq.singleton x
      | [x; y] ->
        match (x.Node.State, y.Node.State) with
        | (Added, Removed) ->
          if x.Actual = y.Expected then Seq.empty
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            Seq.singleton { x with Expected = y.Expected }
        | (Removed, Added) ->
          if x.Expected = y.Actual then Seq.empty
          else
            x.Node.State <- Changed
            y.Node.State <- Changed
            Seq.singleton { x with Actual = y.Actual }
        | (Changed, Changed) ->
          if x.Expected = y.Actual && x.Actual = y.Expected then Seq.empty
          else
            let b = if x.Expected <> null then x.Expected else y.Expected
            let m = if x.Actual <> null then x.Actual else y.Actual
            Seq.singleton
              {
                Node = x.Node
                Expected = b
                Actual = m
              }
        | _ -> seq [x; y]
      | xs ->
        xs
        |> Seq.distinctBy (fun x -> (Path.toStr x.Node.Path, x.Node.State))
    )
    |> Seq.collect (fun x -> translate x.Node x.Expected x.Actual)
    |> fun xs -> { Diff = xs; Ignored = ignored }

type CustomAssertionVisitor =
  inherit NodeVisitor
  abstract member Diff: TranslateResult

module internal Filter =

  let private includedPropertyNames = [
    "FullName"
  ]
  let typ = typeof<Type>
  let filteredTypeProperties =
    typ.GetProperties()
    |> Array.choose (fun x -> if List.exists ((=) x.Name) includedPropertyNames then None else Some x.Name)
  let runtimeType = typ.GetType()
  let filteredRuntimeTypeProperties =
    runtimeType.GetProperties()
    |> Array.choose (fun x -> if List.exists ((=) x.Name) includedPropertyNames then None else Some x.Name)

  let isFilteredProperties t name =
    t = typ && Array.exists ((=) name) filteredTypeProperties || t = runtimeType && Array.exists ((=) name) filteredRuntimeTypeProperties

[<Sealed>]
type internal AssertionVisitor(expectedPrefix: string, expected: obj, actualPrefix: string, actual: obj) =

  let translator = Translator(expectedPrefix, actualPrefix)

  let filter (node: DiffNode) =
    node.IsRootNode && not node.HasChanges
    || node.HasChanges && not node.HasChildren
    || not node.IsRootNode && not (Filter.isFilteredProperties node.ParentNode.Type node.PropertyName) && not node.HasChildren

  member __.Diff = translator.Translate()

  interface CustomAssertionVisitor with
    member this.Diff = this.Diff

  interface NodeVisitor with
    member __.Node(node, _) =
      if filter node then translator.Add(node, node.CanonicalGet(expected), node.CanonicalGet(actual))
