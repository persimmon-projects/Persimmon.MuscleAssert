namespace Persimmon

open System
open Persimmon
open FSharp.Object.Diff

[<NoEquality; NoComparison>]
type Assert(differ: ObjectDiffer, visitor: CustomAssertionVisitor) =
  
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Assert =

  let internal differ =
    let builder =
      ObjectDifferBuilder.StartBuilding()
        .Comparison.OfPrimitiveTypes()
        .ToTreatDefaultValuesAs(Assigned)
        .And()
    builder
      .Differs
      .Register({ new DifferFactory with
        member __.CreateDiffer(dispatcher, service) =
          IEnumerableDiffer(dispatcher, service, builder.Identity) :> Differ
      })
      .Build()

  let equals (expected: 'T) (actual: 'T) =
    Assert(differ, AssertionVisitor("expected", expected, "actual", actual)).equals expected actual
