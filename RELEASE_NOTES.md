### 4.0.1 March 10 2022
- fix to get non generic DU tag in collection

### 4.0.0 March 7 2022
* support netstandard2.0 or higher
* drop net45 and netstandard1.6

### 3.0.0 June 20 2018
* drop support net20, net35, net40 and pcl*
* update Dependencies(Persimmon 4.0.1, Diff.Match.Patch 2.1.0)

### 2.0.0 November 29 2017
* update Dependencies(Persimmon 3.1.1, Diff.Match.Patch 2.0.2)

### 1.0.1 April 10 2017
* disable caller line number [#7](https://github.com/persimmon-projects/Persimmon.MuscleAssert/issues/7)

### 1.0.0 April 7 2017
* support .NET Core
* support PCL(7, 78, 259)
* dump obj type diff
* support mutual recursion object [#5](https://github.com/persimmon-projects/Persimmon.MuscleAssert/issues/5)
* fix to check UseNullAsTrueValue
* fix display path(UseNullAsTrueValue)

### 0.7.2 - September 29 2016
* fix target diff type

### 0.7.1 - September 6 2016
* display null literal [#4](https://github.com/persimmon-projects/Persimmon.MuscleAssert/issues/4)

### 0.7.0 - September 5 2016
* avoid NullReferenceException (UseNullAsTrueValue)
* organize API

### 0.6.0 - June 29 2016
* show normal message if diffs is empty
* show detail string diff

### 0.5.0 - June 3 2016
* rename project
* tuple property show only arity
* trim prefix of DU property

### 0.4.0-beta - June 1 2016
* support seq and System.Collections.IEnumerable(and show ignoring message)
* ignore DU property if union case does not equal

### 0.3.0-beta - May 29 2016
* System.Type compares only FullName property
* support seq and System.Collections.IEnumerable

### 0.2.1-beta - May 28 2016
* fix equality check
* justify bulleted items

### 0.2.0-beta - May 27 2016
* fix target dependencies(FSharp.Object.Diff >= 0.5.2, Persimmon >= 1.1.0)
* change diff prefix
* merge difference of same path
* show chaining style path

### 0.1.0-beta - May 26 2016
* initial release
