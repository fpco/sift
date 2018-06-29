{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

-- |

module Main where

import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Generics
import qualified Data.Graph as Graph
import           Data.List
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import           Options.Applicative.Simple
import           Sift.Types

data TraceOpts = TraceOpts
  { traceOptsPiles :: [FilePath]
  , traceOptsFlaggedIdents :: [BindingId]
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

data Graph = Graph {
  graphGraph :: !Graph.Graph,
  graphVertexToNode :: Graph.Vertex -> (Binding, BindingId, [BindingId]),
  graphLookupVertexByKey :: BindingId -> Maybe Graph.Vertex
 }

main :: IO ()
main = do
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
            (TraceOpts <$> many (strArgument (metavar "BINDINGS_FILE")) <*>
             many
               (option
                  (eitherReader parseBindingId)
                  (long "flag-binding" <> metavar "PKG:MODULE.IDENT" <>
                   help "Flag up this binding")))
          addCommand
            "find"
            "Find"
            findBinding
            (FindOpts <$> many (strArgument (metavar "BINDINGS_FILE")) <*>
             (fmap
                (S8.pack)
                (strOption
                   (long "ident" <> metavar "IDENT" <>
                    help "Find this identifier as a binding")))))
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
    (\(v, binding) -> do
       S.putStrLn ("Flagged binding: " <> prettyBindingId (bindingId binding))
       let inferred = reverseDependencies g v
       if null inferred
         then S.putStrLn "[no uses]"
         else mapM_
                (\(_, bid, _) ->
                   S.putStrLn ("  Used by " <> prettyBindingId bid))
                inferred)
    flagged

applyFlags :: [(BindingId, ByteString)] -> Set Binding -> Set Binding
applyFlags flags bs0 =
  foldl'
    (\bs (bid, flagterm) ->
       let binding =
             Binding
               { bindingId = bid
               , bindingFlagged = Set.singleton flagterm
               , bindingSrcSpan = Nothing
               , bindingRefs = []
               }
        in if Set.member binding bs
             then Set.map
                    (\ebinding ->
                       case lookup (bindingId ebinding) flags of
                         Nothing -> ebinding
                         Just fl ->
                           ebinding
                             { bindingFlagged =
                                 Set.insert fl (bindingFlagged ebinding)
                             })
                    bs
             else Set.insert binding bs)
    bs0
    flags

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

-- | Graph all package bindings.
graphBindings ::
     Set Binding
  -> Graph
graphBindings bs =
  let (g, v2n, vbk) =
        Graph.graphFromEdges
          (map
             (\binding -> (binding, bindingId binding, bindingRefs binding))
             (Set.toList bs))
   in Graph
        {graphGraph = g, graphVertexToNode = v2n, graphLookupVertexByKey = vbk}

-- | Get the bindings that have been flagged up manually.
flaggedVertices :: Graph -> [(Graph.Vertex, Binding)]
flaggedVertices g =
  filter
    (not . Set.null . bindingFlagged . snd)
    (map
       (\v ->
          (let (b, _, _) = graphVertexToNode g v
            in (v, b)))
       (Graph.topSort (graphGraph g)))

-- | Get the reverse dependencies of this vertex.
reverseDependencies :: Graph -> Graph.Vertex -> [(Binding, BindingId, [BindingId])]
reverseDependencies g v =
  map
    (graphVertexToNode g)
    (filter (/= v) (Graph.reachable (Graph.transposeG (graphGraph g)) v))

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

prettyBindingId :: BindingId -> ByteString
prettyBindingId (BindingId pkg md name) = pkg <>  ":" <>  md <>  "." <>  name
