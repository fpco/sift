{-# OPTIONS_GHC -Wno-deprecations #-}
-- |

module Main where

import System.Cmd
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  _ <-
    rawSystem
      "ghc"
      (filter (/= "--interactive") args ++ ["--frontend","Sift.FrontendPlugin"])
  pure ()
