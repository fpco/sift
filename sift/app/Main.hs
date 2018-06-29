{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}

-- |

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import           Options.Applicative.Simple
import           Sift.Types

main :: IO ()
main = do
  (_opts, cmd) <-
    simpleOptions
      "0.0.0"
      "sift"
      "Sift through Haskell modules"
      (pure ())
      (addCommand
         "trace"
         "Trace"
         trace
         (TraceOpts <$> many (strArgument (metavar "BINDINGS_FILE"))))
  cmd

data TraceOpts = TraceOpts
  { traceOptsPiles :: [FilePath]
  }

trace :: TraceOpts -> IO ()
trace opts = do files <- mapM readProfile (traceOptsPiles opts)
                pure ()

readProfile :: FilePath -> IO [Binding]
readProfile fp = do bytes <- L.readFile fp
                    case eitherDecode bytes of
                      Left e  -> error e
                      Right bs -> pure bs

instance FromJSON Binding where
  parseJSON b = do
    o <- parseJSON b
    bindingId <- o .: "id"
    bindingSrcSpan <- o .: "src-span"
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


{-
-- | Track through the module grpah.
trackGraph :: GHC.GhcMonad m => [Binding] -> (GHC.Module -> GHC.HsExpr GHC.Id -> Set String) -> m ()
trackGraph existingBindings shouldFlag = do
  mgraph <- GHC.getModuleGraph
  liftIO
    (putStrLn
       ("Tracking module graph for " ++ show (length mgraph) ++ " module(s) ..."))
  newBindings <- fmap concat (mapM (track shouldFlag) mgraph)
  let (graph, vertexToNode, _lookupVertexByKey) =
        Graph.graphFromEdges
          (map
             (\binding -> (binding, bindingId binding, bindingRefs binding))
             (newBindings ++ existingBindings))
      flaggedVertices :: [(Graph.Vertex, Binding)]
      flaggedVertices =
        filter
          (not . Set.null . bindingFlagged . snd)
          (map
             (\v ->
                (let (b, _, _) = vertexToNode v
                  in (v, b)))
             (Graph.topSort graph))
  liftIO
    (mapM_
       (\(v, judas) -> do
          putStrLn
            ("Flagged binding: " ++
             show (prettyBindingId (bindingId judas)) ++
             ": " ++ intercalate ", " (Set.toList (bindingFlagged judas)))
          let revDeps =
                map
                  vertexToNode
                  (filter (/= v) (Graph.reachable (Graph.transposeG graph) v))
          if not (null revDeps)
            then do
              putStrLn "Which is used by these directly or indirectly:"
              mapM_
                (\(_, bid, _) -> putStrLn ("  " ++ prettyBindingId bid))
                revDeps
            else pure ())
       flaggedVertices)
-}

{-
-- | Track through the module grpah.
_trackThrows :: GHC.GhcMonad m => m ()
_trackThrows = trackGraph existing (const (const mempty))
  where
    existing =
      [ Binding
          { bindingFlagged = Set.singleton "Throws"
          , bindingId =
              (BindingId
                 { bindingIdPackage = "ghc-prim"
                 , bindingIdModule = "GHC.Prim"
                 , bindingIdName = "raise#"
                 })
          , bindingSrcSpan = Nothing
          , bindingRefs = []
          }
      , Binding
          { bindingFlagged = Set.singleton "Unsafe"
          , bindingId =
              (BindingId
                 { bindingIdPackage = "base"
                 , bindingIdModule = "GHC.IO.Unsafe"
                 , bindingIdName = "unsafeDupablePerformIO"
                 })
          , bindingSrcSpan = Nothing
          , bindingRefs = []
          }
      , Binding
          { bindingFlagged = Set.singleton "Unsafe"
          , bindingId =
              (BindingId
                 { bindingIdPackage = "base"
                 , bindingIdModule = "GHC.IO.Unsafe"
                 , bindingIdName = "unsafePerformIO"
                 })
          , bindingSrcSpan = Nothing
          , bindingRefs = []
          }
      ,  Binding
           { bindingFlagged = Set.singleton "Unsafe"
           , bindingId =
               (BindingId
                  { bindingIdPackage = "ghc-prim"
                  , bindingIdModule = "GHC.Prim"
                  , bindingIdName = "unsafeCoerce#"
                  })
           , bindingSrcSpan = Nothing
           , bindingRefs = []
           }
      ]

prettyBindingId :: BindingId -> ByteString
prettyBindingId (BindingId pkg md name) = pkg <>  ":" <>  md <>  "." <>  name

-}
