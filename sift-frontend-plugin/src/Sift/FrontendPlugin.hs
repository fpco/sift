{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend plugin for GHC.

module Sift.FrontendPlugin (frontendPlugin) where

import           Bag
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Data
import           Data.Generics
import qualified Data.Graph as Graph
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified FastString as GHC
import qualified GHC
import qualified GhcPlugins
import qualified Id as GHC
import qualified Module as GHC
import qualified Name as GHC
import           Sift.Types

frontendPlugin :: GhcPlugins.FrontendPlugin
frontendPlugin = GhcPlugins.defaultFrontendPlugin {
  GhcPlugins.frontend = frontend
  }

frontend :: [String] -> [(String, Maybe GHC.Phase)] -> GHC.Ghc ()
frontend _flags args = do
  targets <- mapM (uncurry GHC.guessTarget) args
  GHC.setTargets targets
  _ <- GHC.load GHC.LoadAllTargets
  dumpBindings

-- | Track through the module grpah.
dumpBindings :: GHC.GhcMonad m => m ()
dumpBindings = do
  mgraph <- GHC.getModuleGraph
  mapM_
    (\modSummary -> do
       bs <- track (const (const mempty)) modSummary
       liftIO
         (L.writeFile
            (moduleToFilePath (GHC.ms_mod modSummary))
            (L.toLazyByteString (buildDump bs))))
    mgraph

buildDump :: [Binding] -> L.Builder
buildDump bs = array (map buildBinding bs)

buildBinding :: Binding -> L.Builder
buildBinding b =
  object
    [ ("id", buildBindingId (bindingId b))
    , ("src-span", buildSrcSpan (bindingSrcSpan b))
    , ("refs", array (map buildBindingId (bindingRefs b)))
    ]

buildSrcSpan :: GHC.SrcSpan -> L.Builder
buildSrcSpan s =
  case s of
    GHC.RealSrcSpan rs ->
      object
        [ ("file", string (GHC.unpackFS (GHC.srcSpanFile rs)))
        , ("start-line", int (GHC.srcSpanStartLine rs))
        , ("start-col", int (GHC.srcSpanStartCol rs))
        , ("end-line", int (GHC.srcSpanEndLine rs))
        , ("end-col", int (GHC.srcSpanEndCol rs))
        ]
    GHC.UnhelpfulSpan fs -> string (GHC.unpackFS fs)

buildBindingId :: BindingId -> L.Builder
buildBindingId b =
  object
    [ ("package", string (bindingIdPackage b))
    , ("module", string (bindingIdModule b))
    , ("name", string (bindingIdName b))
    ]

array :: [L.Builder] -> L.Builder
array xs = "[" <> mconcat (intersperse "\n," xs) <> "]"

object :: [(String, L.Builder)] -> L.Builder
object keys =
  "{" <>
  mconcat
    (intersperse
       "\n,"
       (map (\(k, v) -> string k <> ": " <> v) keys)) <>
  "}"

int :: Int -> L.Builder
int s = L.byteString (S8.pack (show s))

string :: String -> L.Builder
string s = L.byteString (S8.pack (show s))

prettyBindingId :: BindingId -> String
prettyBindingId (BindingId pkg md name) = pkg ++ ":" ++ md ++ "." ++ name

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
          , bindingSrcSpan = GHC.UnhelpfulSpan (GHC.mkFastString "")
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
          , bindingSrcSpan = GHC.UnhelpfulSpan (GHC.mkFastString "")
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
          , bindingSrcSpan = GHC.UnhelpfulSpan (GHC.mkFastString "")
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
           , bindingSrcSpan = GHC.UnhelpfulSpan (GHC.mkFastString "")
           , bindingRefs = []
           }
      ]

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
             prettyBindingId (bindingId judas) ++
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

-- | Type-check the module and track through it.
track ::
     GHC.GhcMonad m
  => (GHC.Module -> GHC.HsExpr GHC.Id -> Set String)
  -> GHC.ModSummary
  -> m [Binding]
track shouldFlag modSummary = do
  df <- GHC.getSessionDynFlags
  parsedModule <- GHC.parseModule modSummary
  typecheckedModule <- GHC.typecheckModule parsedModule
  let tc = GHC.tm_typechecked_source typecheckedModule
  pure (getBindingsForAll df tc)
  where
    getBindingsForAll df bag = concatMap (getBinding df) (bagToList bag)
    getBinding ::
         GHC.DynFlags -> GHC.Located (GHC.HsBindLR GHC.Id GHC.Id) -> [Binding]
    getBinding df located =
      case GHC.unLoc located of
        GHC.VarBind {GHC.var_id = id', GHC.var_rhs = rhs} ->
          [ Binding
              { bindingFlagged =
                  mconcat
                    (map
                       (shouldFlag module')
                       (listify (not . Set.null . shouldFlag module') rhs))
              , bindingId = idToBindingId (GHC.ms_mod modSummary) id'
              , bindingSrcSpan = GHC.getLoc located
              , bindingRefs =
                  map
                    (idToBindingId (GHC.ms_mod modSummary) . GHC.unLoc)
                    (referencedIds id' rhs)
              }
          ]
        GHC.FunBind {GHC.fun_id = (GHC.unLoc -> id'), GHC.fun_matches = rhs} ->
          [ Binding
              { bindingFlagged =
                  mconcat
                    (map
                       (shouldFlag module')
                       (listify (not . Set.null . shouldFlag module') rhs))
              , bindingId = idToBindingId module' id'
              , bindingSrcSpan = GHC.getLoc located
              , bindingRefs =
                  map
                    (idToBindingId module' . GHC.unLoc)
                    (referencedIds id' rhs)
              }
          ]
        GHC.AbsBinds {GHC.abs_binds = binds} -> getBindingsForAll df binds
        GHC.AbsBindsSig {GHC.abs_sig_bind = bind} -> getBinding df bind
        GHC.PatBind {} -> []
        GHC.PatSynBind {} -> []
    module' = (GHC.ms_mod modSummary)

-- | Get all the referenced variable IDs listed in an AST.
referencedIds :: Data ast => GHC.Id -> ast -> [GHC.Located GHC.Id]
referencedIds ignore =
  nub .
  mapMaybe
    (\case
       GHC.HsVar i -> Just i
       _ -> Nothing) .
  listify
    (\case
       GHC.HsVar i -> GHC.unLoc i /= ignore
       _ -> False)

idToBindingId :: GHC.Module -> GHC.Id -> BindingId
idToBindingId module0 gid =
  BindingId
    { bindingIdPackage = packageNameVersion
    , bindingIdModule = moduleNameString
    , bindingIdName = nameString
    }
  where
    name = GHC.idName gid
    theOccName = GHC.nameOccName name
    module' = GHC.nameModule_maybe name
    nameString = GHC.occNameString theOccName
    unitId = GHC.moduleUnitId (fromMaybe module0 module')
    moduleName = GHC.moduleName (fromMaybe module0 module')
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName

moduleToFilePath :: GHC.Module -> FilePath
moduleToFilePath module' = "bindings_" ++ packageNameVersion ++ "_" ++ moduleNameString ++ ".json"
  where
    unitId = GHC.moduleUnitId module'
    moduleName = GHC.moduleName module'
    packageNameVersion = GHC.unitIdString unitId
    moduleNameString = GHC.moduleNameString moduleName
