# Persimmon.MuscleAssert

[![NuGet Status](http://img.shields.io/nuget/v/Persimmon.MuscleAssert.svg?style=flat)](https://www.nuget.org/packages/Persimmon.MuscleAssert/)

## Examples

### Plain Persimmon test

code:

```fsharp
type PhoneNumber = {
  CountryCode: string
  AreaCode: string
  LocalNumber: string
}

type Contact = {
  FirstName: string
  LastName: string
  MiddleName: string option
  PhoneNumbers: Dictionary<string, PhoneNumber>
}

type PhoneBook = {
  Name: string
  Contacts: Contact list
}

let example = test {

  let walterPhones = Dictionary<string, PhoneNumber>()
  walterPhones.Add("Home", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "316-7871" })
  walterPhones.Add("Work", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "456-3788" })
  let walterWhite = {
    FirstName = "Walter"
    LastName = "White"
    MiddleName = None
    PhoneNumbers = walterPhones
  }

  let jessePhones = Dictionary<string, PhoneNumber>()
  jessePhones.Add("Home", { CountryCode = "1"; AreaCode = "505"; LocalNumber = "234-4628" })
  let jessePinkman = {
    FirstName = "Jesse"
    LastName = "Pinkman"
    MiddleName = Some ""
    PhoneNumbers = jessePhones
  }

  let expected = {
    Name = "Breaking Bad"
    Contacts =
      [
        walterWhite
        jessePinkman
      ]
  }

  let actual = {
    expected with
      Contacts =
        [
          { walterWhite with MiddleName = Some "Bruce" }
          { jessePinkman with MiddleName = Some "Hartwell" }
        ]
  }

  do! assertEquals expected actual
}
```

result:

```bash
 Assertion Violated: example
  1. Expect: {Name = "Breaking Bad";
      Contacts =
       [{FirstName = "Walter";
         LastName = "White";
         MiddleName = None;
         PhoneNumbers =
          seq
            [[Home, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber];
             [Work, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber]];};
        {FirstName = "Jesse";
         LastName = "Pinkman";
         MiddleName = Some "";
         PhoneNumbers =
          seq [[Home, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber]];}];}
     Actual: {Name = "Breaking Bad";
      Contacts =
       [{FirstName = "Walter";
         LastName = "White";
         MiddleName = Some "Bruce";
         PhoneNumbers =
          seq
            [[Home, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber];
             [Work, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber]];};
        {FirstName = "Jesse";
         LastName = "Pinkman";
         MiddleName = Some "Hartwell";
         PhoneNumbers =
          seq [[Home, Persimmon.MuscleAssert.Tests.AssertTest+Hoge+PhoneNumber]];}];}
```

### Using MuscleAssert

difference:

```diff
+ open Persimmon.MuscleAssert
+
```

result:

```bash
  Assertion Violated: example
  1. .Contacts.[0].MiddleName
       expected FSharpOption<String>.None
       actual   FSharpOption<String>.Some
     .Contacts.[1].MiddleName.Value
       expected
       actual   Hartwell

     @@ -0,0 +1,8 @@
     +Hartwell
```
