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
    (Map.empty, Map.empty |> Map.add 1 0, ["/{1}"; "  + 0"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 1 1, ["/{1}"; "  - 0"; "  + 1"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 1 0 |> Map.add 2 1, ["/{2}"; "  + 1"])
    (Map.empty |> Map.add 1 0, Map.empty |> Map.add 2 1, ["/{2}"; "  + 1"; "/{1}"; "  - 0"])
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
    (dict [], dict [(1, 0)], ["/{1}"; "  + 0"])
    (dict [(1, 0)], dict [(1, 1)], ["/{1}"; "  - 0"; "  + 1"])
    (dict [(1, 0)], dict [(1, 0); (2, 1)], ["/{2}"; "  + 1"])
    (dict [(1, 0)], dict [(2, 1)], ["/{2}"; "  + 1"; "/{1}"; "  - 0"])
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
    (Dictionary<int, int>(), Dictionary<int, int>(dict [(1, 0)]), ["/{1}"; "  + 0"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 1)]), ["/{1}"; "  - 0"; "  + 1"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 0); (2, 1)]), ["/{2}"; "  + 1"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(2, 1)]), ["/{2}"; "  + 1"; "/{1}"; "  - 0"])
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
      ({ A = ["a"]; B = "" }, { A = ["b"]; B = "" }, ["/A/[0]/[0]"; "  + b"; "/A/[0]/[0]"; "  - a"])
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

  let ``dump diff DU`` = test {
    let expected =
      let violated =
        [
          "/Item/B"
          "  - a"
          "  + b"
        ]
        |> String.concat Environment.NewLine
        |> Violated
      NotPassed(violated)
    do!
      Assert.equals (B { A = []; B = "a" }) (B { A = []; B = "b" })
      |> assertEquals expected
  }
