# sift
Sift through Haskell code for analysis purposes

## Install

Install so it's available for any projects using this version of GHC:

```
stack install --copy-compiler-tool
```

## Example use

Use the bindings in `sift-bindings/base-4.9.0.0/` to seed the base
package. Now you can use the `sift trace` command and flag up direct
or indirect uses of a given binding.

```
$ sift trace sift-bindings/*/* --flag-binding "ghc-prim GHC.Prim raise#"
Flagged binding: ghc-prim:GHC.Prim.raise#
  Used by aeson:Data.Aeson.Encoding.Builder.day
  Used by aeson:Data.Aeson.Encoding.Builder.digit
  Used by aeson:Data.Aeson.Encoding.Builder.scientific
  Used by aeson:Data.Aeson.Encoding.Builder.timeOfDay64
  Used by aeson:Data.Aeson.Encoding.Builder.timeZone
  Used by aeson:Data.Aeson.Encoding.Builder.twoDigits
  Used by aeson:Data.Aeson.Internal.Time.diffTimeOfDay64
  Used by aeson:Data.Aeson.Parser.Time.seconds
  Used by aeson:Data.Aeson.Parser.Time.timeZone
  Used by aeson:Data.Aeson.Parser.Unescape.unescapeText'
  [snip]
  Used by base:Control.Exception.Base.noMethodBindingError
  Used by base:Control.Exception.Base.nonExhaustiveGuardsError
  Used by base:Control.Exception.Base.patError
  Used by base:Control.Exception.Base.recConError
  Used by base:Control.Exception.Base.recSelError
  Used by base:Control.Exception.Base.runtimeError
  Used by base:Control.Exception.Base.typeError
  Used by base:Control.Monad.Fix.mfix
  [snip]
```

See
[full gist](https://gist.github.com/chrisdone/143b7bcc1fe21a1cde5d5ce0051f0016)
for full output.

## Discovery

If you're not sure what the proper location for a binding is, search
by identifier:

```
$ sift find sift-bindings/base-4.9.0.0/*.json --ident "fork#"
Binding id: ghc-prim:GHC.Prim.fork#
```

## Generate bindings

For e.g. the aeson package, use sift-compiler in the aeson package directory:

```
$ SIFT_PACKAGE=aeson stack ghci --with-ghc sift-compiler aeson:lib
```

(`cabal repl` should also work similarly!)

That will generate a bunch of files in the current directory for each
module:

```
bindings_main_Data.Aeson.Encode.json
bindings_main_Data.Aeson.Parser.Time.json
bindings_main_Data.Aeson.Encoding.Builder.json
bindings_main_Data.Aeson.Parser.Unescape.json
...
```

Use this to seed `sift` as above.
