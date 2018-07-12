-- | Harness for running the GHC compiler.

module GHCHarness where

import GHC
import GHC.Paths ( libdir )
import DynFlags

compileWith :: FilePath -> Ghc a -> IO a
compileWith fp m =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      _ <- setSessionDynFlags dflags
      target <- guessTarget fp Nothing
      setTargets [target]
      _ <- load LoadAllTargets
      m
