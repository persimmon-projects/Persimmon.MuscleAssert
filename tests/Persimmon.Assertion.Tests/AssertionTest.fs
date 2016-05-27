module Persimmon.Assertion.Tests.AssertionTest

open System
open System.Collections.Generic
open Persimmon
open UseTestNameByReflection

type TestRecord = {
  A: int
}

type TestDU =
  | A
  | B of int
  | C of int * int
  | D of value : int
  | E of int
  | F of TestDU

let ``prefix check`` = test {
  let msg =
    ["/"; "  left 0"; "  right 1"]
    |> String.concat Environment.NewLine
    |> Violated
    |> NotPassed
  do!
    0 === 1
    |> assertEquals msg
}

module Helper =

  let test (expected, actual, message) = test {
    let msg =
      message
      |> String.concat Environment.NewLine
      |> Violated
      |> NotPassed
    do!
      Assert.equals expected actual
      |> assertEquals msg
  }

open Helper

let ``dump diff primitive value`` = test (0, 1, ["/"; "  expected 0"; "  actual 1"])

let ``dump diff string`` = parameterize {
  source [
    ("", "a", ["/"; "  expected "; "  actual a"])
    ("a", "b", ["/"; "  expected a"; "  actual b"])
    ("aaa", "aba", ["/"; "  expected aaa"; "  actual aba"])
  ]
  run test
}

let ``dump diff tuple`` = test ((0, 0), (1, 0), ["/Item1"; "  expected 0"; "  actual 1"])

let ``dump diff record value`` = test ({ A = 0 }, { A = 1 }, ["/A"; "  expected 0"; "  actual 1"])

let ``dump diff list`` = parameterize {
  source [
    ([], [0], ["/[0]"; "  actual 0"])
    ([0], [2], ["/[0]"; "  actual 2"; "/[0]"; "  expected 0"])
    ([0; 1; 3], [0; 2; 3], ["/[1]"; "  actual 2"; "/[1]"; "  expected 1"])
  ]
  run test
}

let ``dump diff array`` = parameterize {
  source [
    ([||], [|1|], ["/[0]"; "  actual 1"])
    ([|1|], [|2|], ["/[0]"; "  actual 2"; "/[0]"; "  expected 1"])
    ([|0; 1; 3|], [|0; 2; 3|], ["/[1]"; "  actual 2"; "/[1]"; "  expected 1"])
  ]
  run test
}

let ``dump diff DU`` = parameterize {
  source [
    (A, B 0, ["/"; "  expected TestDU.A"; "  actual TestDU.B"])
    (B 0, B 1, ["/Item"; "  expected 0"; "  actual 1"])
    (C(0, 1), C(0, 2), ["/Item2"; "  expected 1"; "  actual 2"])
    (D 0, D 1, ["/value"; "  expected 0"; "  actual 1"])
    (B 0, E 1, ["/"; "  expected TestDU.B"; "  actual TestDU.E"])
    (F(B 0), F(B 1), ["/Item/Item"; "  expected 0"; "  actual 1"])
  ]
  run test
}

let ``dump diff Map`` = parameterize {
  source [
    (Map.empty, Map.empty |> Map.add 1 0, ["/{1}"; "  actual 0"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 1 1, ["/{1}"; "  expected 0"; "  actual 1"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 1 0 |> Map.add 2 1, ["/{2}"; "  actual 1"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 2 1, ["/{2}"; "  actual 1"; "/{1}"; "  expected 0"])
  ]
  run test
}

let ``dump diff dict`` = parameterize {
  source [
    (dict [], dict [(1, 0)], ["/{1}"; "  actual 0"])
    (dict [(1, 0)], dict [(1, 1)], ["/{1}"; "  expected 0"; "  actual 1"])
    (dict [(1, 0)], dict [(1, 0); (2, 1)], ["/{2}"; "  actual 1"])
    (dict [(1, 0)], dict [(2, 1)], ["/{2}"; "  actual 1"; "/{1}"; "  expected 0"])
  ]
  run test
}

let ``dump diff Dictionary`` = parameterize {
  source [
    (Dictionary<int, int>(), Dictionary<int, int>(dict [(1, 0)]), ["/{1}"; "  actual 0"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 1)]), ["/{1}"; "  expected 0"; "  actual 1"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 0); (2, 1)]), ["/{2}"; "  actual 1"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(2, 1)]), ["/{2}"; "  actual 1"; "/{1}"; "  expected 0"])
  ]
  run test
}

module Nested =

  type TestRecord = {
    A: string list
    B: string
  }

  type TestDU =
    | A
    | B of TestRecord

  let ``dump diff record value`` = parameterize {
    source [
      ({ A = ["a"]; B = "" }, { A = ["b"]; B = "" }, ["/A[0]"; "  actual b"; "/A[0]"; "  expected a"])
    ]
    run test
  }

  let ``dump diff DU`` = test (B { A = []; B = "a" }, B { A = []; B = "b" }, ["/Item/B"; "  expected a"; "  actual b"])
