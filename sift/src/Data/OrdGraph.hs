-- | A graph built using lists of orderable keys.

module Data.OrdGraph (ordGraph, OrdGraph(..)) where

import Data.Graph

-- | A graph built by a list of instances of 'Ord'.
data OrdGraph key node = OrdGraph {
  ordGraphGraph :: !Graph,
  ordGraphVertexToNode :: Vertex -> (node, key, [key]),
  ordGraphLookupVertexByKey :: key -> Maybe Vertex
 }

-- | Build a graph from a list of nodes uniquely identified by keys,
-- with a list of keys of nodes this node should have edges to. The
-- out-list may contain keys that don't correspond to nodes of the
-- graph; they are ignored.
ordGraph :: Ord key => [(node, key, [key])] -> OrdGraph key node
ordGraph es =
  let (g, v2n, vbk) = graphFromEdges es
   in OrdGraph
           {ordGraphGraph = g, ordGraphVertexToNode = v2n, ordGraphLookupVertexByKey = vbk}
