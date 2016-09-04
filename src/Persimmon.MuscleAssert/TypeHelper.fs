module internal Persimmon.Type

open System

let rec name (t: Type) =
  if t.IsGenericType then
    let n = t.Name.Split([|'`'|]).[0]
    t.GetGenericArguments()
    |> Array.map name
    |> String.concat ", "
    |> sprintf "%s<%s>" n
  else t.Name
