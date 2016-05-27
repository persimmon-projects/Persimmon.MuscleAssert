[<AutoOpen>]
module Persimmon.Assertion

let inline assertEquals expected actual = Assert.equals expected actual

let (===) left right =
  Assert(Assert.differ, AssertionVisitor("left", left, "right", right)).equals left right
