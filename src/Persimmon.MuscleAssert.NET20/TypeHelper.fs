module internal Persimmon.Type

open System
#if PCL || NETSTANDARD
open System.Reflection
#endif

let rec name (t: Type) =
#if PCL || NETSTANDARD
  let info = t.GetTypeInfo()
  if info.IsGenericType then
#else
  if t.IsGenericType then
#endif
    let n = t.Name.Split([|'`'|]).[0]
#if PCL || NETSTANDARD
    info.GenericTypeArguments
#else
    t.GetGenericArguments()
#endif
    |> Array.map name
    |> String.concat ", "
    |> sprintf "%s<%s>" n
  else t.Name
