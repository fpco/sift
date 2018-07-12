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
    "T"
    (it
       ""
       (do bindings <-
             withTempDirectory
               "."
               "sift-temp"
               (\fp -> do
                  setCurrentDirectory fp
                  writeFile "Main.hs" "module Main (main) where main = pure ()"
                  GHC.compileWith "Main.hs" Sift.getBindings)
           shouldBe
             bindings
             [ Binding
                 { bindingId =
                     BindingId
                       { bindingIdPackage = "main"
                       , bindingIdModule = "Main"
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
                       , bindingIdModule = "Main"
                       , bindingIdName = "main"
                       }
                 , bindingFlagged = mempty
                 , bindingSrcSpan =
                     Just
                       (Span
                          { spanFile = "test.hs"
                          , spanStartLine = 1
                          , spanStartCol = 1
                          , spanEndLine = 1
                          , spanEndCol = 14
                          })
                 , bindingRefs =
                     [ BindingId
                         { bindingIdPackage = "base"
                         , bindingIdModule = "GHC.Base"
                         , bindingIdName = "pure"
                         }
                     , BindingId
                         { bindingIdPackage = "ghc-prim"
                         , bindingIdModule = "GHC.Tuple"
                         , bindingIdName = "()"
                         }
                     ]
                 }
             , Binding
                 { bindingId =
                     BindingId
                       { bindingIdPackage = "main"
                       , bindingIdModule = ":Main"
                       , bindingIdName = "main"
                       }
                 , bindingFlagged = mempty
                 , bindingSrcSpan = Nothing
                 , bindingRefs =
                     [ BindingId
                         { bindingIdPackage = "base"
                         , bindingIdModule = "GHC.TopHandler"
                         , bindingIdName = "runMainIO"
                         }
                     , BindingId
                         { bindingIdPackage = "main"
                         , bindingIdModule = "Main"
                         , bindingIdName = "main"
                         }
                     ]
                 }
             ]))
