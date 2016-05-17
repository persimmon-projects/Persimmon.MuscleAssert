[<AutoOpen>]
module Persimmon.Assertion

let assertEquals a b = Assert.equals a b

let inline (===) a b = Assert.equals a b
