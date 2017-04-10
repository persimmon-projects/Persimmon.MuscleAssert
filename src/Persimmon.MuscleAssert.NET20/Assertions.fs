﻿namespace Persimmon

open System
#if NET45 || NETSTANDARD
open System.Runtime.CompilerServices
#endif
open Persimmon
open FSharp.Object.Diff
open Filter

[<NoEquality; NoComparison; Sealed>]
type MuscleAssert(differ: ObjectDiffer, visitor: AssertionVisitor) =
  
  member __.Equal(expected: 'T, actual: 'T,  [<CallerLineNumber>]?line : int) =
    if IEnumerable.isIEnumerable typeof<'T> && IEnumerable.equal expected actual then pass ()
    elif expected = actual then pass ()
    else
      let node = differ.Compare(actual, expected)
      node.Visit(visitor)
      let result = visitor.Translate()
      let msg =
        if Seq.isEmpty result.Diff then
          seq {
            yield! result.Ignored
            yield sprintf "Expect: %A" expected
            yield sprintf "Actual: %A" actual
          }
        else Seq.append result.Diff result.Ignored
        |> String.concat Environment.NewLine
      Assert.Fail(msg, ?line = line)

  static member DefaultDifferBuilder =
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

  static member Equal(expected: 'T, actual: 'T,  [<CallerLineNumber>]?line : int) =
    MuscleAssert(MuscleAssert.DefaultDifferBuilder.Build(), DefaultAssertionVisitor("expected", expected, "actual", actual))
      .Equal(expected, actual, ?line = line)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MuscleAssert =

  let defaultDifferBuilder = MuscleAssert.DefaultDifferBuilder

  let assertEquals (expected: 'T) (actual: 'T) =
    MuscleAssert(defaultDifferBuilder.Build(), DefaultAssertionVisitor("expected", expected, "actual", actual))
      .Equal(expected, actual, ?line = None)

  let (===) left right =
    MuscleAssert(defaultDifferBuilder.Build(), DefaultAssertionVisitor("left", left, "right", right))
      .Equal(left, right, ?line = None)
