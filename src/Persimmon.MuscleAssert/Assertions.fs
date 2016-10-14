namespace Persimmon

open System
open Persimmon
open FSharp.Object.Diff
open Filter

[<NoEquality; NoComparison; Sealed>]
type MuscleAssert(differ: ObjectDiffer, visitor: AssertionVisitor) =
  
  member __.equals (expected: 'T) (actual: 'T) =
    if IEnumerable.isIEnumerable typeof<'T> && IEnumerable.equal expected actual then pass ()
    elif expected = actual then pass ()
    else
      let node = differ.Compare(actual, expected)
      node.Visit(visitor)
      let result = visitor.Translate()
      if Seq.isEmpty result.Diff then
        seq {
          yield! result.Ignored
          yield sprintf "Expect: %A" expected
          yield sprintf "Actual: %A" actual
        }
      else Seq.append result.Diff result.Ignored
      |> String.concat Environment.NewLine
      |> fail

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MuscleAssert =

  let defaultDifferBuilder =
    let builder =
      ObjectDifferBuilder.StartBuilding()
        .Comparison.OfPrimitiveTypes()
        .ToTreatDefaultValuesAs(Assigned)
        .And()
        .Inclusion
        .Exclude()
        .PropertyNameOfType(typ, filteredTypeProperties)
        .And()
        .Filtering
        .ReturnNodesWithState(Ignored)
        .And()
    // System.RuntimeType does not exist mono.
    if typ <> runtimeType then
      builder.Inclusion
        .Exclude()
        .PropertyNameOfType(runtimeType, filteredRuntimeTypeProperties) 
      |> ignore
    builder
      .Differs
      .Register({ new DifferFactory with
        member __.CreateDiffer(_, _) = IEnumerableDiffer :> Differ
      })
      .Differs
      .Register({ new DifferFactory with
        member __.CreateDiffer(dispatcher, service) =
          TupleDiffer(dispatcher, service, service, service, builder.Introspection :?> TypeInfoResolver) :> Differ
      })
      .Differs
      .Register({ new DifferFactory with
        member __.CreateDiffer(dispatcher, service) =
          DiscriminatedUnionDiffer(dispatcher, service, service, service, builder.Introspection :?> TypeInfoResolver) :> Differ
      })

  let private differ = defaultDifferBuilder.Build()

  let assertEquals (expected: 'T) (actual: 'T) =
    MuscleAssert(defaultDifferBuilder.Build(), DefaultAssertionVisitor("expected", expected, "actual", actual)).equals expected actual

  let (===) left right =
    MuscleAssert(defaultDifferBuilder.Build(), DefaultAssertionVisitor("left", left, "right", right)).equals left right
