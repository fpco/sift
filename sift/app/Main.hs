{-# OPTIONS_GHC -Wno-deprecations #-}
-- |

module Main where

import Options.Applicative.Simple

main :: IO ()
main = do
  (_opts, ()) <-
    simpleOptions
      "0.0.0"
      "sift"
      "Sift through Haskell modules"
      (flag () () (long "some-flag"))
      empty
  pure ()
