module Persimmon.Assertion.Tests.AsserionTest

open System
open System.Collections.Generic
open Persimmon
open UseTestNameByReflection

type TestRecord = {
  A: string
}

type TestDU =
  | A
  | B of int
  | C of int * int
  | D of value : string
  | E of string

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

let ``dump diff list`` = parameterize {
  source [
    ([], [0], ["/[0]"; "  + 0"])
    ([0], [2], ["/[0]"; "  + 2"; "/[0]"; "  - 0"])
    ([0; 1; 3], [0; 2; 3], ["/[1]"; "  + 2"; "/[1]"; "  - 1"])
  ]
  run (fun (expected, actual, msg) -> test {
    let msg =
      let violated =
        msg
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals expected actual
      |> assertEquals msg
  })
}

let ``dump diff DU`` = parameterize {
  source [
    (A, B 0, ["/"; "  - TestDU.A"; "  + TestDU.B"])
    (B 0, B 1, ["/Item"; "  - 0"; "  + 1"])
    (C(0, 1), C(0, 2), ["/Item2"; "  - 1"; "  + 2"])
    (D "a", D "b", ["/value"; "  - a"; "  + b"])
    (B 0, E "a", ["/"; "  - TestDU.B"; "  + TestDU.E"])
  ]
  run (fun (expected, actual, msg) -> test {
    let msg =
      let violated =
        msg
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals expected actual
      |> assertEquals msg
  })
}

let ``dump diff Map`` = parameterize {
  source [
    (Map.empty, Map.empty |> Map.add 0 "a", ["/{0}[a]"; "  + a"])
    (Map.empty |> Map.add 0 "a", Map.empty |> Map.add 0 "b", ["/{0}"; "  - a"; "  + b"])
    (Map.empty |> Map.add 0 "a", Map.empty |> Map.add 0 "a" |> Map.add 1 "b", ["/{1}[b]"; "  + b"])
    (Map.empty |> Map.add 0 "a", Map.empty |> Map.add 1 "b", ["/{1}[b]"; "  + b"; "/{0}[a]"; "  - a"])
  ]
  run (fun (expected, actual, msg) -> test {
    let msg =
      let violated =
        msg
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals expected actual
      |> assertEquals msg
  })
}

let ``dump diff dict`` = parameterize {
  source [
    (dict [], dict [(0, "a")], ["/{0}[a]"; "  + a"])
    (dict [(0, "a")], dict [(0, "b")], ["/{0}"; "  - a"; "  + b"])
    (dict [(0, "a")], dict [(0, "a"); (1, "b")], ["/{1}[b]"; "  + b"])
    (dict [(0, "a")], dict [(1, "b")], ["/{1}[b]"; "  + b"; "/{0}[a]"; "  - a"])
  ]
  run (fun (expected, actual, msg) -> test {
    let msg =
      let violated =
        msg
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals expected actual
      |> assertEquals msg
  })
}

let ``dump diff Dictionary`` = parameterize {
  source [
    (Dictionary<int, string>(), Dictionary<int, string>(dict [(0, "a")]), ["/{0}[a]"; "  + a"])
    (Dictionary<int, string>(dict [(0, "a")]), Dictionary<int, string>(dict [(0, "b")]), ["/{0}"; "  - a"; "  + b"])
    (Dictionary<int, string>(dict [(0, "a")]), Dictionary<int, string>(dict [(0, "a"); (1, "b")]), ["/{1}[b]"; "  + b"])
    (Dictionary<int, string>(dict [(0, "a")]), Dictionary<int, string>(dict [(1, "b")]), ["/{1}[b]"; "  + b"; "/{0}[a]"; "  - a"])
  ]
  run (fun (expected, actual, msg) -> test {
    let msg =
      let violated =
        msg
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals expected actual
      |> assertEquals msg
  })
}
