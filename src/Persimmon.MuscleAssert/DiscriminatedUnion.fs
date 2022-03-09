﻿namespace Persimmon

open System
open System.Reflection
open FSharp.Reflection
open FSharp.Object.Diff

module internal DU =

  [<Literal>]
  let Tag = "Tag"

  [<Literal>]
  let Item = "Item"

  let caseName t (c: UnionCaseInfo) = Type.name t + "." + c.Name

[<Sealed>]
type UseNullAsTrueValueElementSelector private () =
  inherit ElementSelector()
  static let instance = UseNullAsTrueValueElementSelector()
  static member Instance = instance :> ElementSelector
  override __.HumanReadableString = ""
  override this.Equals(other) =
    match other with
    | :? RootElementSelector as other when obj.ReferenceEquals(this, other) -> true
    | _ -> other <> null && this.GetType() = other.GetType()
  override __.GetHashCode() = 0

type UseNullAsTrueValueAccessor = {
  Type: Type
  ReturnNullValueName: bool
}
with
  member this.Get(target: obj) =
    if target = null && this.ReturnNullValueName then
      FSharpType.GetUnionCases(this.Type)
      |> Array.pick (fun x ->
        if Array.isEmpty <| x.GetFields() then
          Some(DU.caseName this.Type x)
        else None)
    elif target = null && not this.ReturnNullValueName then null
    else
      let case, _ = FSharpValue.GetUnionFields(target, this.Type)
      DU.caseName this.Type case
  member __.ElementSelector = UseNullAsTrueValueElementSelector.Instance
  interface TypeAwareAccessor with
    member this.ElementSelector = this.ElementSelector
    member this.Get(target) =this.Get(target) |> box
    member __.Set(_, _) = ()
    member this.Type = this.Type
    member __.Unset(_): unit = ()

[<Sealed>]
type UnionCaseItemElementSelector(propertyName: string) =
  inherit ElementSelector()

  override __.HumanReadableString =
    if propertyName.StartsWith(DU.Item) then propertyName.Replace(DU.Item, "")
    else propertyName
  member __.PropertyName = propertyName
  override this.Equals(other) =
    match other with
    | null -> false
    | :? UnionCaseItemElementSelector as other ->
      if obj.ReferenceEquals(this, other) then true
      else this.PropertyName = other.PropertyName
    | _ -> false
  override __.GetHashCode() = hash propertyName

type UnionCaseFieldAccessor =
  | UnionCaseTagAccessor of PropertyInfo
  | UnionCaseItemAccessor of PropertyAwareAccessor
with
  member this.Get(target: obj) =
    match target, this with
    | null, _ -> null
    | _, UnionCaseTagAccessor info ->
      let t = info.DeclaringType
      let tag =
        try
          info.GetValue(target, [||]) |> unbox<int>
        with e ->
          raise <| PropertyReadException(info.Name, target.GetType(), e)
      FSharpType.GetUnionCases(t)
      |> Array.pick (fun x -> if x.Tag = tag then Some(box (DU.caseName t x)) else None)
    | _, UnionCaseItemAccessor accessor -> accessor.Get(target)
  interface PropertyAwareAccessor with
    member this.Type =
      match this with
      | UnionCaseTagAccessor info -> info.PropertyType
      | UnionCaseItemAccessor accessor -> accessor.Type
    member __.GetPropertyAttribute() = null
    member __.PropertyAttributes = Seq.empty
    member this.PropertyName =
      match this with
      | UnionCaseTagAccessor info -> info.Name
      | UnionCaseItemAccessor accessor -> accessor.PropertyName
    member __.Set(_, _) = ()
    member __.Unset(_) = ()
    member __.CategoriesFromAttribute = Set.empty
    member this.Get(target) = this.Get(target)
    member this.ElementSelector =
      match this with
      | UnionCaseTagAccessor info ->
        BeanPropertyElementSelector(info.Name) :> ElementSelector
      | UnionCaseItemAccessor accessor ->
        UnionCaseItemElementSelector(accessor.PropertyName) :> ElementSelector

[<Sealed>]
type DiscriminatedUnionDiffer(
                              differDispatcher: DifferDispatcher,
                              isIntrospectableResolver: IsIntrospectableResolver,
                              isReturnableResolver: IsReturnableResolver,
                              comparisonStrategyResolver: ComparisonStrategyResolver,
                              typeInfoResolver: TypeInfoResolver
  ) =

  let compareUsingIntrospection (node: DiffNode) (instances: Instances) =
    let typeInfo = typeInfoResolver.TypeInfoForNode(node)
    node.TypeInfo <- typeInfo
    for accessor in typeInfo.Accessors do
      let propertyNode = differDispatcher.Dispatch(node, instances, UnionCaseItemAccessor accessor)
      if isReturnableResolver.IsReturnable(propertyNode) then
        node.AddChild(propertyNode)

  let compareUseNullAsTureValue (node: DiffNode) (instances: Instances) returnable =
    let accessor = { Type = instances.Type; ReturnNullValueName = returnable }
    let caseNode = differDispatcher.Dispatch(node, instances, accessor)
    if isReturnableResolver.IsReturnable(caseNode) then
      node.AddChild(caseNode)

  let compareDUTagProperty (node: DiffNode) (instances: Instances) =
    let inner tag =
      let accessor = UnionCaseTagAccessor(tag)
      let propertyNode = differDispatcher.Dispatch(node, instances, accessor)
      if isReturnableResolver.IsReturnable(propertyNode) then
        node.AddChild(propertyNode)

    if FSharpType.IsUnion(instances.Type) then
      let t = instances.Type.GetTypeInfo()
      let tag = t.GetDeclaredProperty(DU.Tag)
      if tag <> null then inner tag
      else
        let superType = t.DeclaringType.GetTypeInfo()
        let tag = superType.GetDeclaredProperty(DU.Tag)
        if tag <> null then
          if superType.IsGenericTypeDefinition then
            // specialize
            superType
              .MakeGenericType(instances.Type.GetGenericArguments())
              .GetTypeInfo()
              .GetDeclaredProperty(DU.Tag)
            |> inner
          else inner tag

  let useNullAsTrueValue (instances: Instances) =
    instances.Type.GetTypeInfo().GetCustomAttributes(false)
    |> Seq.exists (function
    | :? CompilationRepresentationAttribute as a ->
      a.Flags = CompilationRepresentationFlags.UseNullAsTrueValue
    | _ -> false
    )

  let (|HasItem|NoItem|Error|NotCollection|) (instances: Instances, f) =
    let rec inner (instances: Instances) =
      match instances.Parent with
      | Some parent ->
        match instances.SourceAccessor with
        | :? CollectionItemAccessor as accessor ->
          match accessor.TryGet(f parent) with
          | Ok None -> NoItem
          | Ok(Some _) -> HasItem
          | Result.Error e -> Error e
        | _ -> inner parent
      | None -> NotCollection
    inner instances

  let compare (node: DiffNode) (instances: Instances) =
    let getBase (instances: Instances) = instances.Base
    let getWorking (instances: Instances) = instances.Working
    match instances.Base, instances.Working, instances.Parent with
    | null, null, _ -> compareUsingIntrospection node instances
    | _, null, Some _ when useNullAsTrueValue instances ->
      match instances, getWorking with
      | NoItem -> compareUseNullAsTureValue node instances false
      | HasItem -> compareUseNullAsTureValue node instances true
      | Error e -> raise e
      | NotCollection -> compareUseNullAsTureValue node instances true
    | null, _, Some _ when useNullAsTrueValue instances ->
      match instances, getBase with
      | NoItem -> compareUseNullAsTureValue node instances false
      | HasItem -> compareUseNullAsTureValue node instances true
      | Error e -> raise e
      | NotCollection -> compareUseNullAsTureValue node instances true
    | _, null, _ | null, _, _ -> compareDUTagProperty node instances
    | b, w, _ ->
      let baseInfo, _ = FSharpValue.GetUnionFields(b, instances.Type)
      let workingInfo, _ = FSharpValue.GetUnionFields(w, instances.Type)
      if baseInfo <> workingInfo then compareDUTagProperty node instances
      else compareUsingIntrospection node instances

  let compareUsingAppropriateMethod (node: DiffNode) (instances: Instances) =
    let comparisonStrategy = comparisonStrategyResolver.ResolveComparisonStrategy(node)
    if comparisonStrategy <> null then
     comparisonStrategy.Compare(node, instances.Type, instances.Working, instances.Base)
    elif isIntrospectableResolver.IsIntrospectable(node) then
      compare node instances

  member __.Accepts(typ: Type) =
    match Collection.FSharpList.cast typ with
    | None ->
      match Collection.ICollection.cast typ with
      | None when FSharpType.IsUnion(typ) -> true
      | _ -> false
    | _ -> false

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
