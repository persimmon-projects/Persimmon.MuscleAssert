module Persimmon.MuscleAssert.Tests.IEnumerableTest

open System.Collections
open Persimmon
open UseTestNameByReflection

let ``accepts IEnumerable`` = parameterize {
  source [
    typeof<int seq>
    typeof<IEnumerable>
    Seq.empty<int>.GetType()
    (Seq.singleton 1).GetType()
  ]
  run (fun t -> test {
    do! assertPred (IEnumerable.isIEnumerable t)
  })
}

let ``don't accepts implemented IEnumerable types`` = parameterize {
  source [
    typeof<string>
    typeof<int list>
  ]
  run (fun t -> test {
    do! assertPred (not <| IEnumerable.Generic.isSeq t)
  })
}

let ``enumerate seq`` =
  let rec inner acc (e: IEnumerator) =
    if e.MoveNext() then inner (e.Current :: acc) e
    else List.rev acc
  test {
    do!
      seq { 0 .. 2 }
      |> IEnumerable.getEnumerator
      |> inner []
      |> assertEquals (List.map box [0 .. 2])
  }
