module Persimmon.MuscleAssert.Tests.IgnoredPropertyTest

open System
open Persimmon
open UseTestNameByReflection

type TestRecord = {
  A: int
  B: int seq
}

let ``ignore seq`` = test {
  match MuscleAssert.assertEquals Seq.empty (seq { yield 1; yield 2 }) with
  | NotPassed (Violated msg) ->
    let msg = msg.Split([|Environment.NewLine|], StringSplitOptions.None)
    do! assertPred (msg.Length = 3)
    do! assertEquals "  ." msg.[2]
  | a -> do! sprintf "expected NotPassed(Violated msg), but was %A" a |> fail
}

let ``ignore nested seq`` = test {
  match MuscleAssert.assertEquals { A = 0; B = Seq.empty } { A = 0; B = seq { yield 1; yield 2 } } with
  | NotPassed (Violated msg) ->
    let msg = msg.Split([|Environment.NewLine|], StringSplitOptions.None)
    do! assertPred (msg.Length = 3)
    do! assertEquals "  .B" msg.[2]
  | a -> do! sprintf "expected NotPassed(Violated msg), but was %A" a |> fail
}

let ``ignore nested seq included list`` = test {
  match MuscleAssert.assertEquals [{ A = 0; B = Seq.empty }] [{ A = 0; B = seq { yield 1; yield 2 } }] with
  | NotPassed (Violated msg) ->
    let msg = msg.Split([|Environment.NewLine|], StringSplitOptions.None)
    do! assertPred (msg.Length = 3)
    do! assertEquals "  .[0].B" msg.[2]
  | a -> do! sprintf "expected NotPassed(Violated msg), but was %A" a |> fail
}
