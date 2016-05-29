namespace Persimmon

open System
open System.Collections
open System.Collections.Generic
open FSharp.Object.Diff

module IEnumerable =

  let private enumType = typeof<IEnumerable>

  [<RequireQualifiedAccess>]
  module Generic =

    let private runtimeHelpers = Seq.empty<int>.GetType().DeclaringType

    let isSeq (t: Type) =
      if t.IsGenericType then
        let ps = t.GetGenericArguments()
        if Array.length ps = 1 then
          let ie = typedefof<_ seq>.MakeGenericType(ps)
          if ie = t then true
          // System.Type objects of Seq.empty and some generated seq do not equal typeof<'T seq>
          elif ie.IsAssignableFrom(t) && t.DeclaringType = runtimeHelpers then true
          else false
        else false
      else false

  let getEnumerator e =
    enumType.GetMethod("GetEnumerator").Invoke(e, [||])
    :?> IEnumerator

  let isIEnumerable (typ: Type) = typ = typeof<IEnumerable> || Generic.isSeq typ

  let equal (a: obj) (b: obj)  =
    let rec inner (a: IEnumerator) (b: IEnumerator) =
      if a.MoveNext() then
        if b.MoveNext() && a.Current = b.Current then inner a b
        else false
      elif b.MoveNext() then false
      else true
    inner (getEnumerator a) (getEnumerator b)

type IEnumerableWrapper =
  | NonGenericIEnumerable of IEnumerable
  | SeqWrapper of obj
with
  interface IEnumerable with
    member this.GetEnumerator() =
      match this with
      | NonGenericIEnumerable e -> e.GetEnumerator()
      | SeqWrapper o -> IEnumerable.getEnumerator o

[<AutoOpen>]
module IEnumerableSyntax =

  let (|IsIEnumerable|_|) (o: obj) =
    match o with
    | :? IEnumerable as o -> Some(NonGenericIEnumerable o)
    | _ when IEnumerable.Generic.isSeq (o.GetType()) -> Some(SeqWrapper o)
    | _ -> None

type SeqItemAccessor(referenceItem: obj, index: int option, identityStrategy: IdentityStrategy) =

  let objectAsCollection: obj -> Choice<IEnumerableWrapper option, ArgumentException> = function
  | null -> Choice1Of2 None
  | IsIEnumerable e -> Choice1Of2(Some e)
  | o -> Choice2Of2(ArgumentException(o.GetType().FullName))

  new(referenceItem) = SeqItemAccessor(referenceItem, None, EqualsIdentityStrategy :> IdentityStrategy)

  member __.Index = index

  member __.ElementSelector =
    let selector = CollectionItemElementSelector(referenceItem, index)
    if identityStrategy = null then selector else selector.WithIdentityStrategy(identityStrategy)
    :> ElementSelector

  member private __.TryGet(target: obj) =

    let rec inner (e: IEnumerator) =
      if e.MoveNext() then
        if e.Current <> null && identityStrategy.Equals(e.Current, referenceItem) then
          Some e.Current
        else inner e
      else None
        
    match objectAsCollection target with
    | Choice1Of2 None -> Choice1Of2 None
    | Choice1Of2(Some cs) -> (cs :> IEnumerable).GetEnumerator() |> inner |> Choice1Of2
    | Choice2Of2 e -> Choice2Of2 e

  member this.Get(target: obj) =
    match this.TryGet(target) with
    | Choice1Of2 None -> null
    | Choice1Of2(Some v) -> v
    | Choice2Of2 e -> raise e

  override this.ToString() =
    "seq item " + this.ElementSelector.ToString()

  member __.Type = if referenceItem <> null then referenceItem.GetType() else null

  interface TypeAwareAccessor with
    member this.Type = this.Type
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) = this.Get(target)
    member this.Set(target, value) = raise <| NotSupportedException("seq<'T> is immutable.")
    member this.Unset(target) = raise <| NotSupportedException("seq<'T> is immutable.")

type IEnumerableDiffer(
                       differDispatcher: DifferDispatcher,
                       comparisonStrategyResolver: ComparisonStrategyResolver,
                       identityStrategyResolver: IdentityStrategyResolver) =

  let compareUsingComparisonStrategy (node: DiffNode) (instances: Instances) (comparisonStrategy: ComparisonStrategy) =
    comparisonStrategy.Compare(
      node,
      instances.Type,
      (
        match instances.Working with
        | IsIEnumerable(NonGenericIEnumerable v) -> box v
        | IsIEnumerable(SeqWrapper v) -> v
        | _ -> null
      ),
      match instances.Base with
      | IsIEnumerable(NonGenericIEnumerable v) -> box v
      | IsIEnumerable(SeqWrapper v) -> v
      | _ -> null
    )

  let contains (haystack: IEnumerable) needle (identityStrategy: IdentityStrategy) =
    let rec inner (e: IEnumerator) =
      if e.MoveNext() then 
        if identityStrategy.Equals(needle, e.Current) then true
        else inner e
      else false
    inner (haystack.GetEnumerator())

  let remove (from: ResizeArray<int * obj>) these identityStrategy =
    from.RemoveAll(fun (_, item) -> contains these item identityStrategy)
    |> ignore

  let compareItems (node: DiffNode) (instances: Instances) (items: (int * obj) seq) identityStrategy =
    items
    |> Seq.iter (fun (index, o) ->
      let accessor = SeqItemAccessor(o, Some index, identityStrategy)
      differDispatcher.Dispatch(node, instances, accessor)
      |> ignore
    )

  let getOrEmpty (o: obj) =
    match o with
    | IsIEnumerable xs ->
      Some(xs :> IEnumerable)
    | _ -> None
    |> Option.map (fun xs ->
      let cs = ResizeArray()
      for x in xs do cs.Add(x)
      cs :> obj seq
    )
    |> function
    | Some v -> v |> Seq.mapi (fun i o -> (i, o))
    | None -> Seq.empty

  let compareInternally (node: DiffNode) (instances: Instances) identityStrategy =
    let working = instances.Working |> getOrEmpty
    let base_ = instances.Base |> getOrEmpty

    let added = ResizeArray(working)
    let removed = ResizeArray(base_)
    let known = ResizeArray(base_)

    remove added base_ identityStrategy
    remove removed working identityStrategy
    remove known added identityStrategy
    remove known removed identityStrategy

    compareItems node instances added identityStrategy
    compareItems node instances removed identityStrategy
    compareItems node instances known identityStrategy

  interface Differ with
    member __.Accepts(typ) = IEnumerable.isIEnumerable typ
    member __.Compare(parentNode, instances) =
      let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
      let identityStrategy = identityStrategyResolver.ResolveIdentityStrategy(node)
      if identityStrategy <> null then
        node.ChildIdentityStrategy <- identityStrategy
      if instances.HasBeenAdded then
        let addedItems = instances.Working |> getOrEmpty
        compareItems node instances addedItems identityStrategy
        node.State <- Added
      elif instances.HasBeenRemoved then
        let removedItems = instances.Base |> getOrEmpty
        compareItems node instances removedItems identityStrategy
        node.State <- Removed
      elif instances.AreSame then
        node.State <- Untouched
      else
        let comparisonStrategy = comparisonStrategyResolver.ResolveComparisonStrategy(node)
        if comparisonStrategy = null then
          compareInternally node instances identityStrategy
        else compareUsingComparisonStrategy node instances comparisonStrategy
      node
