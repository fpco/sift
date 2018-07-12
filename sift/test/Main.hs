{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import           Control.Exception
import           Data.OrdGraph
import qualified Data.Set as Set
import qualified GHCHarness as GHC
import           Sift
import qualified Sift.FrontendPlugin as Sift
import           Sift.Types
import           System.Directory
import           System.IO.Temp
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe
    "Compiling and generating bindings"
    (do singleFile
        dependentModules)
  describe "Tracing"
           traceit

traceit :: SpecWith ()
traceit =
  it
    "Trace single module"
    (filesShouldTrace
       [ ("Foo.hs", "module Foo (foo) where foo = foo")
       , ("Bar.hs", "module Bar (bar) where import Foo; bar = Foo.foo")
       ]
       [BindingId
          { bindingIdPackage = "main"
          , bindingIdModule = "Foo"
          , bindingIdName = "foo"
          }]
       [BindingId {bindingIdPackage = "main", bindingIdModule = "Bar", bindingIdName = "bar"}])

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

----------------------------------------------------------------------
-- Combinators

filesShouldBe :: [(FilePath, String)] -> [Binding] -> IO ()
filesShouldBe files expected = do
  bindings <- getBindings files
  shouldBe bindings expected

filesShouldTrace :: [(FilePath, String)] -> [BindingId] -> [BindingId] -> IO ()
filesShouldTrace files bids expected = do
  bindings0 <- getBindings files
  let !bindings =
        applyFlags (map (, "flag-binding") bids) (Set.fromList bindings0)
      !g = graphBindings bindings
      flagged = flaggedVertices g
  shouldBe
    (concatMap (map (snd3 . ordGraphVertexToNode g) . infer g . fst) flagged)
    expected
  where
    snd3 (_, k, _) = k

getBindings :: [(FilePath, String)] -> IO [Binding]
getBindings files = do
  pwd <- getCurrentDirectory
  bindings <-
    withTempDirectory
      "."
      "sift-temp"
      (\dir ->
         finally
           (do setCurrentDirectory dir
               mapM_ (\(name, contents) -> writeFile name contents) files
               GHC.compileWith (map fst files) Sift.getBindings)
           (do setCurrentDirectory pwd
               removeDirectoryRecursive dir))
  pure bindings
