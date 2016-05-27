[<AutoOpen>]
module Persimmon.Assertion

let inline assertEquals a b = Assert.equals a b

let (===) a b =
  Assert(Assert.differ, AssertionVisitor("left", a, "right", b)).equals a b
