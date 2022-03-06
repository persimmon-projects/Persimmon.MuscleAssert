module internal Persimmon.Type

open System
open System.Reflection

let rec name (t: Type) =
  let info = t.GetTypeInfo()
  if info.IsGenericType then
    let n = t.Name.Split([|'`'|]).[0]
    info.GenericTypeArguments
    |> Array.map name
    |> String.concat ", "
    |> sprintf "%s<%s>" n
  else t.Name
