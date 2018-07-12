{-# LANGUAGE OverloadedStrings #-}

-- |

module Sift where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Graph as Graph
import           Data.List
import           Data.Monoid
import           Data.Ord
import           Data.OrdGraph
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           Sift.Types

infer :: Ord k => OrdGraph k v -> Graph.Vertex -> [Graph.Vertex]
infer g flagged = sortBy
                    (comparing ((\(_, y, _) -> y) . ordGraphVertexToNode g))
                    (reverseDependencies g flagged)

prettyBindingId :: BindingId -> ByteString
prettyBindingId (BindingId pkg md name) = pkg <>  ":" <>  md <>  "." <>  name

callTrace :: OrdGraph BindingId node -> Graph.Vertex -> Graph.Vertex -> [Tree [Char]]
callTrace g start flagged =
  fmap
    (fmap
       (\v' ->
          let (_, bid', _) = ordGraphVertexToNode g v'
           in S8.unpack (prettyBindingId bid')))
    (filterForest
       (flip (Graph.path (ordGraphGraph g)) flagged)
       (Graph.dfs (ordGraphGraph g) [start]))

filterForest :: (t -> Bool) -> [Tree t] -> [Tree t]
filterForest p xs =
  if null xs'
     then []
     else filter (p . rootLabel) xs'
  where xs' = map (\(Node l ys) -> Node l (filterForest p ys)) xs

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

-- | Graph all package bindings.
graphBindings ::
     Set Binding
  -> OrdGraph BindingId Binding
graphBindings bs =
  ordGraph (map
              (\binding -> (binding, bindingId binding, bindingRefs binding))
              (Set.toList bs))

-- | Get the bindings that have been flagged up manually.
flaggedVertices :: OrdGraph k Binding -> [(Graph.Vertex, Binding)]
flaggedVertices g =
  filter
    (not . Set.null . bindingFlagged . snd)
    (map
       (\v ->
          (let (b, _, _) = ordGraphVertexToNode g v
            in (v, b)))
       (Graph.topSort (ordGraphGraph g)))

-- | Get the reverse dependencies of this vertex.
reverseDependencies :: OrdGraph k v -> Graph.Vertex -> [Graph.Vertex]
reverseDependencies g v =
  filter (/= v) (Graph.reachable (Graph.transposeG (ordGraphGraph g)) v)
