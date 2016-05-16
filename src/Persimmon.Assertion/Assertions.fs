module Persimmon.Assert

open System
open Persimmon
open FSharp.Object.Diff

let equals (a: 'T) (b: 'T) =
  if a = b then pass ()
  else
    let node =
      ObjectDifferBuilder.StartBuilding()
        .Comparison.OfPrimitiveTyoes()
        .ToTreatDefaultValuesAs(Assigned)
        .And()
        .Build()
        .Compare(b, a)
    let visitor = AssertionVisitor(b, a)
    node.Visit(visitor)
    fail visitor.Diff
