namespace Persimmon.MuscleAssert.Tests

open Persimmon
open UseTestNameByReflection

module UseNullAsTrueValueAccessorTest =

  let ``provide access to its path element`` = test {
    let accessor = { Type = typeof<int option>; ReturnNullValueName = false }
    do! assertPred (accessor.ElementSelector :? UseNullAsTrueValueElementSelector)
  }

  let ``get UseNullAsTrueValue name`` = test {
    let accessor = { Type = typeof<int option>; ReturnNullValueName = true }
    do! assertEquals (box "FSharpOption<Int32>.None") (accessor.Get(None))
  }
