module Persimmon.MuscleAssert.Tests.AssertionTest

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

let ``pass seq test`` = test {
  do!
    MuscleAssert.assertEquals (seq { 0 .. 2 }) (seq { 0 .. 2})
    |> assertEquals (Passed ())
}

let ``prefix check`` = test {
  let cause =
    ["."; "  left  0"; "  right 1"]
    |> String.concat Environment.NewLine
    |> Violated
  let msg = NotPassed(None, cause)
  do!
    MuscleAssert.(===) 0 1
    |> assertEquals msg
}

module Helper =

  let test (expected, actual, message) = test {
    let cause =
      message
      |> String.concat "\n"
      |> Violated
    let msg = NotPassed(None, cause)
    do!
      match MuscleAssert.assertEquals expected actual with
      | NotPassed(l, Violated cause) -> NotPassed(l, Violated(cause.Replace("\r\n", "\n")))
      | v -> v
      |> assertEquals msg
  }

  let expected v = sprintf "  expected %O" v
  let actual v = sprintf "  actual   %O" v

open Helper

let ``dump diff primitive value`` = test (0, 1, ["."; expected 0; actual 1])

let ``dump diff string`` = parameterize {
  source [
    ("", "a", ["."; expected ""; actual "a"; ""; "@@ -0,0 +1 @@"; "+a"; ""])
    (null, "a", ["."; expected "null"; actual "a"])
    ("a", null, ["."; expected "a"; actual "null"])
    ("a", "b", ["."; expected "a"; actual "b"; ""; "@@ -1 +1 @@"; "-a"; "+b"; ""])
    ("aaa", "aba", ["."; expected "aaa"; actual "aba"; ""; "@@ -1,3 +1,3 @@"; " a"; "-a"; "+b"; " a"; ""])
  ]
  run test
}

let ``dump diff tuple`` = parameterize {
  source [
    (box (0, 0), box (1, 0), [".1"; expected 0; actual 1])
    (box (0, 0, 0), box (1, 0, 2), [".1"; expected 0; actual 1; ".3"; expected 0; actual 2])
    (box (0, 0, 0, 0, 0, 0, 0, 0), box (0, 0, 0, 0, 0, 0, 0, 1), [".8"; expected 0; actual 1])
    (box (0, 0, 0, 0, 0, 0, 0, 0, 0), box (0, 0, 0, 0, 0, 0, 0, 0, 1), [".9"; expected 0; actual 1])
    (box (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), box (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), [".16"; expected 0; actual 1])
  ]
  run test
}

let ``dump diff record value`` = test ({ A = 0 }, { A = 1 }, [".A"; expected 0; actual 1])

let ``dump diff list`` = parameterize {
  source [
    ([], [1], [".[0]"; "  actual 1"])
    ([1], [], [".[0]"; "  expected 1"])
    ([0], [2], [".[0]"; expected 0; actual 2])
    ([0; 1; 3], [0; 2; 3], [".[1]"; expected 1; actual 2])
  ]
  run test
}

let ``dump diff array`` = parameterize {
  source [
    ([||], null, ["."; expected "System.Int32[]"; actual "null"])
    (null, [||], ["."; expected "null"; actual "System.Int32[]"])
    ([||], [|1|], [".[0]"; "  actual 1"])
    ([|1|], [||], [".[0]"; "  expected 1"])
    ([|1|], [|2|], [".[0]"; expected 1; actual 2])
    ([|0; 1; 3|], [|0; 2; 3|], [".[1]"; expected 1; actual 2])
  ]
  run test
}

let ``dump diff Set`` = parameterize {
  source [
    (Set.empty, Set.ofList [1], [".[0]"; "  actual 1"])
    (Set.ofList [1], Set.empty, [".[0]"; "  expected 1"])
    (Set.ofList [1], Set.ofList [2], [".[0]"; expected 1; actual 2])
    (Set.ofList [0; 1; 3], Set.ofList [0; 2; 3], [".[1]"; expected 1; actual 2])
  ]
  run test
}

let ``dump diff DU`` = parameterize {
  source [
    (A, B 0, [".";  expected "TestDU.A"; actual "TestDU.B"])
    (B 0, B 1, ["."; expected 0; actual 1])
    (C(0, 1), C(0, 2), [".2"; expected 1; actual 2])
    (C(1, 0), C(2, 0), [".1"; expected 1; actual 2])
    (D 0, D 1, [".value"; expected 0; actual 1])
    (B 0, E 1, ["."; expected "TestDU.B"; actual "TestDU.E"])
    (F(B 0), F(B 1), ["."; expected 0; actual 1])
  ]
  run test
}

let ``dump diff Map`` = parameterize {
  source [
    (Map.empty, Map.ofList [(1, 0)], [".{1}"; "  actual 0"])
    (Map.ofList [(1, 0)], Map.empty, [".{1}"; "  expected 0"])
    (Map.ofList [(1, 0)], Map.ofList [(1, 1)], [".{1}"; expected 0; actual 1])
    (Map.ofList [(1, 0)], Map.ofList [(1, 0); (2, 1)], [".{2}"; "  actual 1"])
    (Map.ofList [(1, 0)], Map.ofList [(2, 1)], [".{2}"; "  actual 1"; ".{1}"; "  expected 0"])
  ]
  run test
}

let ``dump diff dict`` = parameterize {
  source [
    (dict [], dict [(1, 0)], [".{1}"; "  actual 0"])
    (dict [(1, 0)], dict [(1, 1)], [".{1}"; expected 0; actual 1])
    (dict [(1, 0)], dict [(1, 0); (2, 1)], [".{2}"; "  actual 1"])
    (dict [(1, 0)], dict [(2, 1)], [".{2}"; "  actual 1"; ".{1}"; "  expected 0"])
  ]
  run test
}

let ``dump diff Dictionary`` = parameterize {
  source [
    (Dictionary<int, int>(), Dictionary<int, int>(dict [(1, 0)]), [".{1}"; "  actual 0"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 1)]), [".{1}"; expected 0; actual 1])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(1, 0); (2, 1)]), [".{2}"; "  actual 1"])
    (Dictionary<int, int>(dict [(1, 0)]), Dictionary<int, int>(dict [(2, 1)]), [".{2}"; "  actual 1"; ".{1}"; "  expected 0"])
  ]
  run test
}

let ``System.Type dump only FullName`` =
  test (typeof<int>, typeof<string>, [".FullName"; expected "System.Int32"; actual "System.String"; ""; "@@ -4,9 +4,10 @@"; " tem."; "-Int32"; "+String"; ""])

module Nested =

  type TestRecord = {
    X: string list
    Y: int
  }

  type TestDU =
    | A
    | B of TestRecord
    | C
    | D of (int * string)

  let ``dump diff record includeing list`` = parameterize {
    source [
      ({ X = ["a"]; Y = 1 }, { X = ["b"]; Y = 1 }, [".X.[0]"; expected "a"; actual "b"; ""; "@@ -1 +1 @@"; "-a"; "+b"; ""])
    ]
    run test
  }

  let ``dump diff DU includeing record`` =
    test (B { X = []; Y = 1 }, B { X = []; Y = 2 }, [".Y"; expected 1; actual 2])

  let ``dump diff DU includeing tuple`` =
    test (D (0, "a"), D (0, "b"), [".2"; expected "a"; actual "b"; ""; "@@ -1 +1 @@"; "-a"; "+b"; ""])

  let ``dump diff list including record`` = parameterize {
    source [
      ([ { X = [] ; Y = 1 } ], [ { X = []; Y = 2 } ], [".[0].Y"; expected 1; actual 2])
      ([ { X = ["a"; "B"]; Y = 1 } ], [ { X = ["A"; "B"]; Y = 1 } ], [".[0].X.[0]"; expected "a"; actual "A"; ""; "@@ -1 +1 @@"; "-a"; "+A"; ""])
    ]
    run test
  }

  let ``dump diff Map including list`` = parameterize {
    source [
      (Map.ofList [ (1, []) ], Map.ofList [ (1, [2]) ], [".{1}.[0]"; "  actual 2"])
      (Map.ofList [ (1, [1]) ], Map.ofList [ (1, [2]) ], [".{1}.[0]"; expected 1; actual 2])
    ]
    run test
  }

  let ``dump diff list includeing DU`` = parameterize {
    source [
      ([A], [], [".[0]"; expected "TestDU.A"])
      ([A; A], [A; C], [".[1]"; expected "TestDU.A"; actual "TestDU.C"])
      ([A], [B { X = []; Y = 2 }], [".[0]"; expected "TestDU.A"; actual "TestDU.B"])
      ([B { X = []; Y = 1 }], [B { X = []; Y = 2 }], [".[0].Y"; expected 1; actual 2])
    ]
    run test
  }

  let ``dump diff tuple`` = parameterize {
    source [
      ([], [box (1, 0)], [".[0].1"; "  actual 1"; ".[0].2"; "  actual 0"])
      ([box (0, 0)], [box (1, 0)], [".[0].1"; expected 0; actual 1])
      ([box (0, 0); box (0, 0)], [box (0, 0); box (1, 0)], [".[1].1"; expected 0; actual 1])
      ([box (0, 0, 0, 0, 0, 0, 0, 0)], [box (0, 0, 0, 0, 0, 0, 0, 1)], [".[0].8"; expected 0; actual 1])
    ]
    run test
  }

  type V = V of string option
  
  let ``dump diff list included CompilationRepresentationFlags.UseNullAsTrueValue`` =
    test ([ V (Some "b") ], [], [".[0]"; expected "V.V"])

  let ``allow CompilationRepresentationFlags.UseNullAsTrueValue included CompilationRepresentationFlags.UseNullAsTrueValue`` =
    test ([ Some (Some "") ], [], [".[0]"; expected "FSharpOption<FSharpOption<String>>.Some"])

  type T = { X: string option }

  let ``dump diff list included CompilationRepresentationFlags.UseNullAsTrueValue 2`` =
    test ([], [ { X = Some "a" } ], [".[0].X"; actual "FSharpOption<String>.Some"])

module Generic =

  let ``allow CompilationRepresentationFlags.UseNullAsTrueValue`` =
    test (Some 0, None, ["."; expected "FSharpOption<Int32>.Some"; actual "FSharpOption<Int32>.None"])

  type Either<'T, 'U> =
    | Left of 'T
    | Right of 'U

  let ``show multiple generic parameter`` =
    test (Left 0, Right "1", ["."; expected "Either<Int32, String>.Left"; actual "Either<Int32, String>.Right"])

  module Nested =

    let ``dump list included DU`` =
      let xs: Either<Either<int, string>, string> list =  [Left (Left 0)]
      test (xs, [], [".[0]"; expected "Either<Either<Int32, String>, String>.Left"])

module Recursion =

  open System.Xml.Linq

  let ``avoid infinite loop`` =
    let a =
      XElement(
        XName.Get("a"),
        XElement(
          XName.Get("B"),
          0
        )
      )
    let b =
      XElement(
        XName.Get("a"),
        XElement(
          XName.Get("B"),
          1
        )
      )
    test (a, b, [".Value"; expected 0; actual 1; ""; "@@ -1 +1 @@"; "-0"; "+1"; ""])

module Box =

  [<CustomEquality; NoComparison>]
  type A = {
    A: obj
    B: string
  }
  with
    override x.Equals(y: obj) =
      match y with
      | :? A as y -> x.A = y.A
      | _ -> false
    override x.GetHashCode() = hash x.A

  let ``dump diff obj`` =
    test ({ A = 2; B = null }, { A = "2"; B = "x" }, [".A"; expected 2; actual "2"; ".B"; expected "null"; actual "x"])
