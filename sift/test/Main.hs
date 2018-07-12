{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import qualified GHCHarness as GHC
import qualified Sift.FrontendPlugin as Sift
import           Sift.Types
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Compiling and generating bindings"
    (do singleFile
        dependentModules)

dependentModules :: SpecWith ()
dependentModules =
  it
    "Dependent modules"
    (filesShouldBe
       [ ("Foo.hs", "module Foo (foo) where foo = foo")
       , ("Bar.hs", "module Bar (bar) where import Foo; bar = Foo.foo")
       ]
       [ Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Bar"
                 , bindingIdName = "$trModule"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan = Nothing
           , bindingRefs =
               [ BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "Module"
                   }
               , BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "TrNameS"
                   }
               ]
           }
       , Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Bar"
                 , bindingIdName = "bar"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan =
               Just
                 (Span
                    { spanFile = "Bar.hs"
                    , spanStartLine = 1
                    , spanStartCol = 36
                    , spanEndLine = 1
                    , spanEndCol = 49
                    })
           , bindingRefs =
               [ BindingId
                   { bindingIdPackage = "main"
                   , bindingIdModule = "Foo"
                   , bindingIdName = "foo"
                   }
               ]
           }
       , Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Foo"
                 , bindingIdName = "$trModule"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan = Nothing
           , bindingRefs =
               [ BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "Module"
                   }
               , BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "TrNameS"
                   }
               ]
           }
       , Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Foo"
                 , bindingIdName = "foo"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan =
               Just
                 (Span
                    { spanFile = "Foo.hs"
                    , spanStartLine = 1
                    , spanStartCol = 24
                    , spanEndLine = 1
                    , spanEndCol = 33
                    })
           , bindingRefs = []
           }
       ])

singleFile :: SpecWith ()
singleFile =
  it
    "Basic single file"
    (filesShouldBe
       [("Foo.hs", "module Foo (foo) where foo = foo")]
       [ Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Foo"
                 , bindingIdName = "$trModule"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan = Nothing
           , bindingRefs =
               [ BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "Module"
                   }
               , BindingId
                   { bindingIdPackage = "ghc-prim"
                   , bindingIdModule = "GHC.Types"
                   , bindingIdName = "TrNameS"
                   }
               ]
           }
       , Binding
           { bindingId =
               BindingId
                 { bindingIdPackage = "main"
                 , bindingIdModule = "Foo"
                 , bindingIdName = "foo"
                 }
           , bindingFlagged = mempty
           , bindingSrcSpan =
               Just
                 (Span
                    { spanFile = "Foo.hs"
                    , spanStartLine = 1
                    , spanStartCol = 24
                    , spanEndLine = 1
                    , spanEndCol = 33
                    })
           , bindingRefs = []
           }
       ])

filesShouldBe :: [(FilePath, String)] -> [Binding] -> IO ()
filesShouldBe files expected = do
  bindings <-
    withTempDirectory
      "."
      "sift-temp"
      (\fp -> do
         setCurrentDirectory fp
         mapM_ (\(name, contents) -> writeFile name contents) files
         GHC.compileWith (map fst files) Sift.getBindings)
  shouldBe bindings expected
