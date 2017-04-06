namespace Persimmon

open System
#if PCL || NETSTANDARD
open System.Reflection
#endif
open System.Collections
open System.Collections.Generic
open FSharp.Object.Diff

module internal IEnumerable =

  let private enumType = typeof<IEnumerable>

  [<RequireQualifiedAccess>]
  module Generic =

    let private runtimeHelpers = Seq.empty<int>.GetType().DeclaringType

    let isSeq (t: Type) =
#if PCL || NETSTANDARD
      let info = t.GetTypeInfo()
      if info.IsGenericType then
#else
      if t.IsGenericType then
#endif
        let ps =
#if PCL || NETSTANDARD
          info.GenericTypeArguments
#else
          t.GetGenericArguments()
#endif
        if Array.length ps = 1 then
          let ie =
            typedefof<_ seq>
#if PCL || NETSTANDARD
              .GetTypeInfo()
#endif
              .MakeGenericType(ps)
          if ie = t then true
          // System.Type objects of Seq.empty and some generated seq do not equal typeof<'T seq>
          else
            ie
#if PCL || NETSTANDARD
              .GetTypeInfo()
              .IsAssignableFrom(info)
#else
              .IsAssignableFrom(t)
#endif
              && t.DeclaringType = runtimeHelpers
        else false
      else false

  let getEnumerator e =
    enumType
#if PCL || NETSTANDARD
      .GetTypeInfo()
      .GetDeclaredMethod("GetEnumerator")
#else
      .GetMethod("GetEnumerator")
#endif
      .Invoke(e, [||])
    :?> IEnumerator

  let isIEnumerable (typ: Type) = typ = typeof<IEnumerable> || Generic.isSeq typ

  let equal (a: obj) (b: obj)  =
    let rec inner (a: IEnumerator) (b: IEnumerator) =
      if a.MoveNext() then
        if b.MoveNext() && a.Current = b.Current then inner a b
        else false
      else not <| b.MoveNext()
    inner (getEnumerator a) (getEnumerator b)

type internal IEnumerableWrapper =
  | NonGenericIEnumerable of IEnumerable
  | SeqWrapper of obj
with
  interface IEnumerable with
    member this.GetEnumerator() =
      match this with
      | NonGenericIEnumerable e -> e.GetEnumerator()
      | SeqWrapper o -> IEnumerable.getEnumerator o

[<AutoOpen>]
module internal IEnumerableSyntax =

  let (|IsIEnumerable|_|) (o: obj) =
    match o with
    | :? IEnumerable as o -> Some(NonGenericIEnumerable o)
    | _ when IEnumerable.Generic.isSeq (o.GetType()) -> Some(SeqWrapper o)
    | _ -> None

type IEnumerableDiffer = IEnumerableDiffer
with
  interface Differ with
    member __.Accepts(typ) = IEnumerable.isIEnumerable typ
    member __.Compare(parentNode, instances) =
      let node = DiffNode(parentNode, instances.SourceAccessor, instances.Type)
      node.State <- Ignored
      node
