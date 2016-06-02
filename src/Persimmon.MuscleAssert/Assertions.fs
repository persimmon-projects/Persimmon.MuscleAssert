﻿module Persimmon.MuscleAssert

open System
open Persimmon
open FSharp.Object.Diff

[<NoEquality; NoComparison>]
type CustomMuscleAssert(differ: ObjectDiffer, visitor: CustomAssertionVisitor) =
  
  member __.equals (expected: 'T) (actual: 'T) =
    if IEnumerable.isIEnumerable typeof<'T> && IEnumerable.equal expected actual then pass ()
    elif expected = actual then pass ()
    else
      let node = differ.Compare(actual, expected)
      node.Visit(visitor)
      let diff = visitor.Diff
      if String.IsNullOrEmpty diff then
        sprintf "Expect: %A%sActual: %A" expected Environment.NewLine actual
      else diff
      |> fail

open Filter

let internal differ =
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
    .Build()

let assertEquals (expected: 'T) (actual: 'T) =
  CustomMuscleAssert(differ, AssertionVisitor("expected", expected, "actual", actual)).equals expected actual

let (===) left right =
  CustomMuscleAssert(differ, AssertionVisitor("left", left, "right", right)).equals left right