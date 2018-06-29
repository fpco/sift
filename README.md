# sift
Sift through Haskell code for analysis purposes

## Example use

Use the bindings in `sift-bindings/base-4.9.0.0/` to seed the base
package. Now you can use the `sift trace` command and flag up direct
or indirect uses of a given binding.

```
$ sift trace sift-bindings/base-4.9.0.0/* --flag-binding "ghc-prim GHC.Prim raise#"
Flagged binding: ghc-prim:GHC.Prim.raise#
  Used by base:GHC.Real.ratioZeroDenominatorError
  Used by base:GHC.Real.reduce
  Used by base:GHC.Real.-
  Used by base:GHC.Real.+
  Used by base:GHC.Real.*
  Used by base:GHC.Real.%
  Used by base:GHC.Word.toRational
  Used by base:GHC.Real.fromRational
  Used by base:GHC.Real.realToFrac
  Used by base:GHC.Read.convertFrac
  Used by base:GHC.Float.round
  Used by base:GHC.Float.recip
  Used by base:GHC.Float.floor
  Used by base:GHC.Float.ceiling
  Used by base:GHC.Float.atanh
  Used by base:GHC.Float.asinh
  Used by base:GHC.Float.acosh
  Used by base:GHC.Event.TimerManager.updateTimeout
  Used by base:GHC.Event.TimerManager.registerTimeout
  Used by base:GHC.Event.Thread.threadDelay
  Used by base:GHC.Conc.IO.threadDelay
[snip]
```

See
[full gist](https://gist.github.com/chrisdone/df28017801d4e91691ed0b17bef5f82b)
for full output.
