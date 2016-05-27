namespace Persimmon

open System
open Persimmon
open FSharp.Object.Diff

[<NoEquality; NoComparison>]
type Assert(differ: ObjectDiffer, visitor: CustomAssertionVisitor) =
  
  member __.equals (expected: 'T) (actual: 'T) =
    if expected = actual then pass ()
    else
      let node = differ.Compare(actual, expected)
      node.Visit(visitor)
      fail visitor.Diff

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Assert =

  let internal differ =
    ObjectDifferBuilder.StartBuilding()
      .Comparison.OfPrimitiveTypes()
      .ToTreatDefaultValuesAs(Assigned)
      .And()
      .Build()

  let equals (expected: 'T) (actual: 'T) =
    Assert(differ, AssertionVisitor("expected", expected, "actual", actual)).equals expected actual
