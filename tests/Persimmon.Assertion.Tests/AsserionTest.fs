module Persimmon.Assertion.Tests.AsserionTest

open System
open Persimmon
open UseTestNameByReflection

type TestRecord = {
  A: string
}

let ``dump diff primitive value`` = test {
  let expected =
    let violated =
      [
        "/"
        "  - 0"
        "  + 1"
      ]
      |> String.concat Environment.NewLine
      |> Violated
    NotPassed(violated)
  do!
    Assert.equals 0 1
    |> assertEquals expected
}

let ``dump diff string`` = test {
  let expected =
    let violated =
      [
        "/"
        "  - a"
        "  + b"
      ]
      |> String.concat Environment.NewLine
      |> Violated
    NotPassed(violated)
  do!
    Assert.equals "a" "b"
    |> assertEquals expected
}

let ``dump diff record value`` = test {
  let expected =
    let violated =
      [
        "/A"
        "  - a"
        "  + b"
      ]
      |> String.concat Environment.NewLine
      |> Violated
    NotPassed(violated)
  do!
    Assert.equals { A = "a" } { A = "b" }
    |> assertEquals expected
}
