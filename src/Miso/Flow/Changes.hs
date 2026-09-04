-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Changes
-- License     :  BSD3-style (see the file LICENSE)
--
-- Applying 'NodeChange' \/ 'EdgeChange' lists to your model, plus
-- selection-change helpers. The change types themselves live in
-- "Miso.Flow.Types" (port of @types\/changes.ts@); the application logic
-- follows the framework packages' @applyChanges@.
----------------------------------------------------------------------------
module Miso.Flow.Changes
  ( applyNodeChanges
  , applyEdgeChanges
  , nodeChangeId
  , edgeChangeId
  , getSelectionChanges
  , selectNodes
  , unselectNodesAndEdges
  ) where
-----------------------------------------------------------------------------
import qualified Data.Set as S
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | The node id a change refers to (the new item's id for adds).
nodeChangeId :: NodeChange n -> NodeId
nodeChangeId = \case
  NodeDimensionChange i _ _ _ -> i
  NodePositionChange i _ _ _ -> i
  NodeSelectionChange i _ -> i
  NodeRemoveChange i -> i
  NodeAddChange item _ -> nodeId item
  NodeReplaceChange i _ -> i
-----------------------------------------------------------------------------
-- | The edge id a change refers to (the new item's id for adds).
edgeChangeId :: EdgeChange e -> EdgeId
edgeChangeId = \case
  EdgeSelectionChange i _ -> i
  EdgeRemoveChange i -> i
  EdgeAddChange item _ -> edgeId item
  EdgeReplaceChange i _ -> i
-----------------------------------------------------------------------------
-- | Apply a list of node changes to a node array; mirrors
-- @applyNodeChanges@ from the framework packages.
applyNodeChanges :: [NodeChange n] -> [Node n] -> [Node n]
applyNodeChanges changes nodes = foldl (flip applyOne) nodes changes
  where
    applyOne (NodeAddChange item mIndex) ns =
      case mIndex of
        Nothing -> ns <> [item]
        Just i -> take i ns <> [item] <> drop i ns
    applyOne (NodeRemoveChange i) ns =
      [ n | n <- ns, nodeId n /= i ]
    applyOne (NodeReplaceChange i item) ns =
      [ if nodeId n == i then item else n | n <- ns ]
    applyOne c ns =
      [ if nodeId n == nodeChangeId c then patch c n else n | n <- ns ]
    patch (NodeSelectionChange _ selected) n = n { nodeSelected = selected }
    patch (NodePositionChange _ mPos _ mDragging) n = n
      { nodePosition = maybe (nodePosition n) id mPos
      , nodeDragging = maybe (nodeDragging n) id mDragging
      }
    patch (NodeDimensionChange _ mDims mResizing setAttrs) n =
      case mDims of
        Nothing -> resized
        Just (Dimensions w h) -> resized
          { nodeMeasured = Just (Measured (Just w) (Just h))
          , nodeWidth = case setAttrs of
              SetAttributesBoth -> Just w
              SetAttributesWidth -> Just w
              _ -> nodeWidth n
          , nodeHeight = case setAttrs of
              SetAttributesBoth -> Just h
              SetAttributesHeight -> Just h
              _ -> nodeHeight n
          }
      where
        resized = maybe n (\r -> n { nodeResizing = r }) mResizing
    patch _ n = n
-----------------------------------------------------------------------------
-- | Apply a list of edge changes to an edge array; mirrors
-- @applyEdgeChanges@ from the framework packages.
applyEdgeChanges :: [EdgeChange e] -> [Edge e] -> [Edge e]
applyEdgeChanges changes edges = foldl (flip applyOne) edges changes
  where
    applyOne (EdgeAddChange item mIndex) es =
      case mIndex of
        Nothing -> es <> [item]
        Just i -> take i es <> [item] <> drop i es
    applyOne (EdgeRemoveChange i) es =
      [ e | e <- es, edgeId e /= i ]
    applyOne (EdgeReplaceChange i item) es =
      [ if edgeId e == i then item else e | e <- es ]
    applyOne (EdgeSelectionChange i selected) es =
      [ if edgeId e == i then e { edgeSelected = selected } else e | e <- es ]
-----------------------------------------------------------------------------
-- | Selection changes needed to make exactly the given ids selected;
-- mirrors @getSelectionChanges@.
getSelectionChanges
  :: [(NodeId, Bool)]
  -- ^ (id, currently selected) for every element
  -> S.Set NodeId
  -- ^ ids that should be selected
  -> [NodeChange n]
getSelectionChanges elements selectedIds =
  [ NodeSelectionChange i willBeSelected
  | (i, isSelected) <- elements
  , let willBeSelected = i `S.member` selectedIds
  , isSelected /= willBeSelected
  ]
-----------------------------------------------------------------------------
-- | Mark exactly the given node ids as selected.
selectNodes :: S.Set NodeId -> [Node n] -> [Node n]
selectNodes ids =
  map (\n -> n { nodeSelected = nodeId n `S.member` ids })
-----------------------------------------------------------------------------
-- | Deselect all (or the given) nodes and edges; mirrors
-- @unselectNodesAndEdges@.
unselectNodesAndEdges
  :: Maybe [NodeId]
  -> Maybe [EdgeId]
  -> ([Node n], [Edge e])
  -> ([Node n], [Edge e])
unselectNodesAndEdges mNodeIds mEdgeIds (nodes, edges) =
  ( [ if keepNode n then n { nodeSelected = False } else n | n <- nodes ]
  , [ if keepEdge e then e { edgeSelected = False } else e | e <- edges ]
  )
  where
    keepNode n = maybe True (elem (nodeId n)) mNodeIds
    keepEdge e = maybe True (elem (edgeId e)) mEdgeIds
-----------------------------------------------------------------------------
