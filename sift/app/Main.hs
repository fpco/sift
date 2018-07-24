{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

-- |

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Generics
import           Data.List
import           Data.Monoid
import           Data.OrdGraph
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import           Data.Tree
import qualified Data.Yaml
import           Options.Applicative.Simple
import           Paths_sift
import           Sift
import           Sift.Types

data TraceOpts = TraceOpts
  { traceOptsPiles :: [FilePath]
  , traceOptsFlaggedIdents :: [BindingId]
  , traceOptsShowCallTrace :: Bool
  }

data AuditOpts = AuditOpts
  { auditOptsPiles :: [FilePath]
  , auditOptsFlagFile :: FilePath
  , auditOptsPackage :: !(Maybe ByteString)
  , auditOptsShowCallTrace :: Bool
  }

data FindOpts = FindOpts
  { findOptsPiles :: [FilePath]
  , findOptsIdent :: ByteString
  }

instance FromJSON Binding where
  parseJSON b = do
    o <- parseJSON b
    bindingId <- o .: "id"
    bindingSrcSpan <- o .:? "src-span"
    bindingRefs <- o .: "refs"
    let bindingFlagged = mempty
    pure Binding {..}

instance FromJSON Span where
  parseJSON b = do
    o <- parseJSON b
    spanFile <- fmap T.encodeUtf8 (o .: "file")
    spanStartLine <- (o .: "start-line")
    spanStartCol <- (o .: "start-col")
    spanEndLine <- (o .: "end-line")
    spanEndCol <- (o .: "end-col")
    pure Span {..}

instance FromJSON BindingId where
  parseJSON b = do
    o <- parseJSON b
    bindingIdPackage <- fmap T.encodeUtf8 (o .: "package")
    bindingIdModule <- fmap T.encodeUtf8 (o .: "module")
    bindingIdName <- fmap T.encodeUtf8 (o .: "name")
    pure BindingId {..}

main :: IO ()
main = do
  flagfile <- getDataFileName "flag.yaml"
  (_opts, cmd) <-
    simpleOptions
      "0.0.0"
      "sift"
      "Sift through Haskell modules"
      (pure ())
      (do addCommand
            "trace"
            "Trace"
            trace
            (TraceOpts <$> many (strArgument (metavar "BINDINGS_FILE.json")) <*>
             many
               (option
                  (eitherReader parseBindingId)
                  (long "flag-binding" <> metavar "PKG:MODULE.IDENT" <>
                   help "Flag up this binding")) <*>
             flag
               False
               True
               (help "Show call trace (output tends to be large)" <>
                long "call-trace"))
          addCommand
            "find"
            "Find"
            findBinding
            (FindOpts <$> many (strArgument (metavar "BINDINGS_FILE.json")) <*>
             (fmap
                (S8.pack)
                (strOption
                   (long "ident" <> metavar "IDENT" <>
                    help "Find this identifier as a binding"))))
          addCommand
            "audit"
            "Produce a report of general checks for a codebase"
            auditReport
            (AuditOpts <$> many (strArgument (metavar "BINDINGS_FILE.json")) <*>
             strArgument (metavar "FLAG_FILE.yaml" <> value flagfile) <*>
             optional (fmap S8.pack (strOption (metavar "PACKAGE_NAME" <> long "package"))) <*>
             flag
               False
               True
               (help "Show call trace (output tends to be large)" <>
                long "call-trace")))
  cmd

findBinding :: FindOpts -> IO ()
findBinding opts = do
  bindings <- readProfiles (findOptsPiles opts)
  mapM_
    (\binding ->
       S.putStrLn ("Binding id: " <> prettyBindingId (binding)))
    (nub (listify ((== findOptsIdent opts) . bindingIdName) bindings))

trace :: TraceOpts -> IO ()
trace opts = do
  bindings0 <- readProfiles (traceOptsPiles opts)
  let !bindings =
        applyFlags
          (map (, "flag-binding") (traceOptsFlaggedIdents opts))
          bindings0
      !g = graphBindings bindings
      flagged = flaggedVertices g
  mapM_
    (\(fl, binding) -> do
       S.putStrLn ("Flagged binding: " <> prettyBindingId (bindingId binding))
       let inferred = infer g fl
       if null inferred
         then S.putStrLn "[no uses]"
         else mapM_
                (\start -> do
                   let (_, bid, _) = ordGraphVertexToNode g start
                   S.putStrLn ("  Used by " <> prettyBindingId bid)
                   when
                     (traceOptsShowCallTrace opts)
                     (putStr
                        (unlines
                           (map
                              ("  " ++)
                              (["Call trace:"] ++
                               lines (drawForest (callTrace g start fl)))))))
                inferred)
    flagged

auditReport :: AuditOpts -> IO ()
auditReport opts = do
  bindings0 <- readProfiles (auditOptsPiles opts)
  iffy <-
    do r <- Data.Yaml.decodeFileEither (auditOptsFlagFile opts)
       case r of
         Left e -> throw e
         Right k -> pure k
  let !bindings = applyFlags (map (, "flag-binding") iffy) bindings0
      !g = graphBindings bindings
      flagged = flaggedVertices g
  mapM_
    (\(fl, binding) -> do
       S.putStrLn ("Flagged binding: " <> prettyBindingId (bindingId binding))
       let inferred = infer g fl
       if null inferred
         then S.putStrLn "[no uses]"
         else mapM_
                (\start -> do
                   let (bind, bid, _) = ordGraphVertexToNode g start
                   when
                     (maybe
                        True
                        (bindingIdPackage (bindingId bind) ==)
                        (auditOptsPackage opts))
                     (do S.putStrLn ("  Used by " <> prettyBindingId bid)
                         when
                           (auditOptsShowCallTrace opts)
                           (putStr
                              (unlines
                                 (map
                                    ("  " ++)
                                    (["Call trace:"] ++
                                     lines (drawForest (callTrace g start fl))))))))
                inferred)
    flagged

readProfiles :: [FilePath] -> IO (Set Binding)
readProfiles fps = do
  files <- mapM readProfile fps
  pure (Set.fromList (concat files))

readProfile :: FilePath -> IO [Binding]
readProfile fp = do
  bytes <- L.readFile fp
  case eitherDecode bytes of
    Left e -> error e
    Right bs -> pure bs

parseBindingId :: String -> Either String BindingId
parseBindingId s =
  case words s of
    [pkg, m, i] ->
      pure (BindingId
              { bindingIdPackage = S8.pack pkg
              , bindingIdModule = S8.pack m
              , bindingIdName = S8.pack i
              })
    _ -> Left "format: package module ident (e.g. base Prelude fmap)"
