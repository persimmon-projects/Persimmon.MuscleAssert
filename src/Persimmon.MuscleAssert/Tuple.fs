namespace Persimmon

open System
open FSharp.Reflection
open FSharp.Object.Diff

[<Sealed>]
type TupleItemElementSelector(propertyName: string, arity: int) =
  inherit ElementSelector()

  override __.HumanReadableString = string arity
  member __.PropertyName = propertyName
  member __.Arity = arity
  override this.Equals(other) =
    match other with
    | null -> false
    | :? TupleItemElementSelector as other ->
      if obj.ReferenceEquals(this, other) then true
      else this.PropertyName = other.PropertyName
    | _ -> false
  override __.GetHashCode() = hash propertyName

type TupleItemAccessor = {
  Accessor: PropertyAwareAccessor
  N: int
}
with
  interface PropertyAwareAccessor with
    member this.Type = this.Accessor.Type
    member this.GetPropertyAttribute<'T when 'T :> Attribute and 'T : null>() = this.Accessor.GetPropertyAttribute<'T>()
    member this.PropertyAttributes = this.Accessor.PropertyAttributes
    member this.PropertyName = this.Accessor.PropertyName
    member this.Set(target, value) = this.Accessor.Set(target, value)
    member this.Unset(target) = this.Accessor.Unset(target)
    member this.CategoriesFromAttribute = this.Accessor.CategoriesFromAttribute
    member this.Get(target) = this.Accessor.Get(target)
    member this.ElementSelector = TupleItemElementSelector(this.Accessor.PropertyName, this.N) :> ElementSelector

[<Sealed>]
type TupleDiffer(
                 differDispatcher: DifferDispatcher,
                 isIntrospectableResolver: IsIntrospectableResolver,
                 isReturnableResolver: IsReturnableResolver,
                 comparisonStrategyResolver: ComparisonStrategyResolver,
                 typeInfoResolver: TypeInfoResolver
  ) =

  let compareUsingIntrospection (node: DiffNode) (beanInstances: Instances) =
    let typeInfo = typeInfoResolver.TypeInfoForNode(node)
    node.TypeInfo <- typeInfo
    typeInfo.Accessors
    |> List.iteri (fun index accessor ->
      let arity = index + 1
      let propertyNode = differDispatcher.Dispatch(node, beanInstances, { Accessor = accessor; N = arity })
      if isReturnableResolver.IsReturnable(propertyNode) then
        node.AddChild(propertyNode)
    )

  let compareUsingAppropriateMethod (node: DiffNode) (instances: Instances) =
    let comparisonStrategy = comparisonStrategyResolver.ResolveComparisonStrategy(node)
    if comparisonStrategy <> null then
     comparisonStrategy.Compare(node, instances.Type, instances.Working, instances.Base)
    elif isIntrospectableResolver.IsIntrospectable(node) then
      compareUsingIntrospection node instances

  member __.Accepts(typ: Type) = FSharpType.IsTuple(typ)

  member __.Compare(parentNode, instances: Instances) =
    let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
    if instances.AreNull || instances.AreSame then
      node.State <- Untouched
    elif instances.HasBeenAdded then
      compareUsingAppropriateMethod node instances
      node.State <- Added
    elif instances.HasBeenRemoved then
      compareUsingAppropriateMethod node instances
      node.State <- Removed
    else compareUsingAppropriateMethod node instances
    node

  interface Differ with

    member this.Accepts(typ: Type) = this.Accepts(typ)

    member this.Compare(parentNode, instances) = this.Compare(parentNode, instances)
