﻿namespace System
open System.Reflection
open System.Runtime.InteropServices

[<assembly: AssemblyTitleAttribute("Persimmon.Assertion")>]
[<assembly: AssemblyProductAttribute("Persimmon.Assertion")>]
[<assembly: GuidAttribute("42384685-8f4f-45d7-a305-0528113e1c19")>]
[<assembly: AssemblyDescriptionAttribute("")>]
[<assembly: AssemblyVersionAttribute("0.3.0")>]
[<assembly: AssemblyFileVersionAttribute("0.3.0")>]
[<assembly: AssemblyInformationalVersionAttribute("0.3.0-beta")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.3.0"
    let [<Literal>] InformationalVersion = "0.3.0-beta"
