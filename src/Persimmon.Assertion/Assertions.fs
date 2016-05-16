module Persimmon.Assert

open System
open Persimmon
open FSharp.Object.Diff

let equals (a: 'T) (b: 'T) =
  if a = b then pass ()
  elif Type.isPrimitive(typeof<'T>) then
    [
      "/"
      String.indent 1 "- " + a.ToString()
      String.indent 1 "+ " + b.ToString()
    ]
    |> String.concat Environment.NewLine
    |> fail
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
