namespace Persimmon

open System
open Persimmon
open FSharp.Object.Diff

[<NoEquality; NoComparison>]
type Assert(differ: ObjectDiffer, visitor: CustomAssertionVisitor) =
  
  member __.equals (a: 'T) (b: 'T) =
    if a = b then pass ()
    else
      let node = differ.Compare(b, a)
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

  let equals (a: 'T) (b: 'T) =
    Assert(differ, AssertionVisitor("expected", a, "actual", b)).equals a b
