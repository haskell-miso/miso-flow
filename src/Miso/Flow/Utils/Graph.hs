-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Graph
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/graph.ts@ from @\@xyflow\/system@.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Graph
  ( getOutgoers
  , getIncomers
  , getNodePositionWithOrigin
  , getNodesBounds
  , getNodesBoundsWithLookup
  , getInternalNodesBounds
  , getNodesInside
  , getConnectedEdges
  , getFitViewNodes
  , fitViewportFor
  , calculateNodePosition
  , getElementsToRemove
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, mapMaybe, isJust)
import qualified Data.Set as S
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Flow.Constants (infiniteExtent)
import           Miso.Flow.Types
import           Miso.Flow.Utils.General
-----------------------------------------------------------------------------
-- | Nodes connected to the given node as the target of an edge.
getOutgoers :: NodeId -> [Node n] -> [Edge e] -> [Node n]
getOutgoers nid nodes edges =
  let outgoerIds = S.fromList [ edgeTarget e | e <- edges, edgeSource e == nid ]
  in [ n | n <- nodes, nodeId n `S.member` outgoerIds ]
-----------------------------------------------------------------------------
-- | Nodes connected to the given node as the source of an edge.
getIncomers :: NodeId -> [Node n] -> [Edge e] -> [Node n]
getIncomers nid nodes edges =
  let incomerIds = S.fromList [ edgeSource e | e <- edges, edgeTarget e == nid ]
  in [ n | n <- nodes, nodeId n `S.member` incomerIds ]
-----------------------------------------------------------------------------
-- | Position of a node adjusted by its origin.
getNodePositionWithOrigin :: Node n -> NodeOrigin -> XYPosition
getNodePositionWithOrigin n nodeOrigin' =
  let Dimensions w h = getNodeDimensions n
      NodeOrigin ox oy = fromMaybe nodeOrigin' (nodeOrigin n)
  in XYPosition
      (xyX (nodePosition n) - w * ox)
      (xyY (nodePosition n) - h * oy)
-----------------------------------------------------------------------------
-- | Bounding box containing all given nodes (no lookup: positions are
-- taken from the user nodes and the given origin).
getNodesBounds :: [Node n] -> NodeOrigin -> Rect
getNodesBounds [] _ = Rect 0 0 0 0
getNodesBounds nodes origin =
  boxToRect (foldl step emptyBox nodes)
  where
    step acc n = getBoundsOfBoxes acc (nodeToBox n origin)
-----------------------------------------------------------------------------
-- | Bounding box containing all the given node ids, resolved through a
-- 'NodeLookup' so sub-flow (parent) positions are correct. Unknown ids
-- are skipped.
getNodesBoundsWithLookup :: [NodeId] -> NodeLookup n -> Rect
getNodesBoundsWithLookup ids nodeLookup =
  case mapMaybe (`M.lookup` nodeLookup) ids of
    [] -> Rect 0 0 0 0
    found -> boxToRect (foldl (\acc n -> getBoundsOfBoxes acc (internalNodeToBox n)) emptyBox found)
-----------------------------------------------------------------------------
-- | Bounding box of the internal nodes matching the filter.
getInternalNodesBounds
  :: (InternalNode n -> Bool)
  -> NodeLookup n
  -> Rect
getInternalNodesBounds p nodeLookup =
  case filter p (M.elems nodeLookup) of
    [] -> Rect 0 0 0 0
    visible ->
      boxToRect (foldl (\acc n -> getBoundsOfBoxes acc (internalNodeToBox n)) emptyBox visible)
-----------------------------------------------------------------------------
emptyBox :: Box
emptyBox = Box (1/0) (1/0) (-1/0) (-1/0)
-----------------------------------------------------------------------------
-- | Internal nodes inside (or partially inside) the given rect.
getNodesInside
  :: NodeLookup n
  -> Rect
  -> Transform
  -> Bool
  -- ^ partially: also include nodes only partially inside
  -> Bool
  -- ^ exclude non-selectable nodes
  -> [InternalNode n]
getNodesInside nodeLookup rect (Viewport tx ty tScale) partially excludeNonSelectable =
  [ n | n <- M.elems nodeLookup, keep n (internalUser n) ]
  where
    paneX = (rectX rect - tx) / tScale
    paneY = (rectY rect - ty) / tScale
    paneWidth = rectWidth rect / tScale
    paneHeight = rectHeight rect / tScale
    keep n u
      | excludeNonSelectable && not (fromMaybe True (nodeSelectable u)) = False
      | nodeHidden u = False
      | otherwise =
          let Dimensions w h = getInternalNodeDimensions n
              XYPosition x y = internalPositionAbsolute n
              overlappingArea =
                getRectsOverlappingArea paneX paneY paneWidth paneHeight x y w h
              area = w * h
              partiallyVisible = partially && overlappingArea > 0
              forceInitialRender = internalHandleBounds n == Nothing
              isVisible =
                forceInitialRender || partiallyVisible || overlappingArea >= area
          in isVisible || nodeDragging u
-----------------------------------------------------------------------------
-- | Edges where either endpoint is one of the given nodes.
getConnectedEdges :: [Node n] -> [Edge e] -> [Edge e]
getConnectedEdges nodes edges =
  let nodeIds = S.fromList (map nodeId nodes)
  in [ e | e <- edges
     , edgeSource e `S.member` nodeIds || edgeTarget e `S.member` nodeIds ]
-----------------------------------------------------------------------------
-- | Which nodes participate in a fit-view; port of @getFitViewNodes@.
getFitViewNodes :: NodeLookup n -> FitViewOptions -> NodeLookup n
getFitViewNodes nodeLookup opts =
  M.filter keep restricted
  where
    restricted = case fitViewNodes opts of
      Nothing -> nodeLookup
      Just ids ->
        let idSet = S.fromList ids
        in M.filterWithKey (\k _ -> k `S.member` idSet) nodeLookup
    keep n
      | fitViewIncludeHiddenNodes opts =
          let Dimensions w h = getInternalNodeDimensions n
          in w > 0 && h > 0
      | otherwise =
          isJust (measuredWidth (internalMeasured n))
            && measuredWidth (internalMeasured n) /= Just 0
            && isJust (measuredHeight (internalMeasured n))
            && measuredHeight (internalMeasured n) /= Just 0
            && not (nodeHidden (internalUser n))
-----------------------------------------------------------------------------
-- | Pure core of @fitViewport@: computes the viewport for fitting the
-- given nodes into @width@ × @height@. Returns 'Nothing' for an empty
-- lookup (the effectful caller then leaves the viewport unchanged).
fitViewportFor
  :: NodeLookup n
  -> Double
  -- ^ width
  -> Double
  -- ^ height
  -> Double
  -- ^ min zoom
  -> Double
  -- ^ max zoom
  -> FitViewOptions
  -> Maybe Viewport
fitViewportFor nodeLookup width height minZoom maxZoom opts
  | M.null nodeLookup = Nothing
  | otherwise =
      let nodesToFit = getFitViewNodes nodeLookup opts
          bounds = getInternalNodesBounds (const True) nodesToFit
      in Just $ getViewportForBounds
           bounds
           width
           height
           (fromMaybe minZoom (fitViewMinZoom opts))
           (fromMaybe maxZoom (fitViewMaxZoom opts))
           (fitViewPadding opts)
-----------------------------------------------------------------------------
-- | Next position of a node given its extent, parent and origin; port of
-- @calculateNodePosition@. Returns @(position, positionAbsolute)@;
-- 'Nothing' when the node is missing from the lookup.
calculateNodePosition
  :: NodeId
  -> XYPosition
  -- ^ next (absolute) position
  -> NodeLookup n
  -> NodeOrigin
  -> Maybe CoordinateExtent
  -- ^ global node extent
  -> Maybe (XYPosition, XYPosition)
calculateNodePosition nid nextPosition nodeLookup nodeOrigin' nodeExtent' = do
  n <- M.lookup nid nodeLookup
  let u = internalUser n
      parentNode = (`M.lookup` nodeLookup) =<< nodeParentId u
      XYPosition parentX parentY =
        maybe zeroPosition internalPositionAbsolute parentNode
      NodeOrigin ox oy = fromMaybe nodeOrigin' (nodeOrigin u)
      extent =
        case nodeExtent u of
          Just ExtentParent
            | not (nodeExpandParent u) ->
                case parentNode of
                  Nothing -> ExtentCoordinates <$> nodeExtent'
                  Just parent ->
                    let Dimensions pw ph = getInternalNodeDimensions parent
                    in if pw /= 0 && ph /= 0
                         then Just $ ExtentCoordinates $ CoordinateExtent
                                parentX parentY (parentX + pw) (parentY + ph)
                         else ExtentCoordinates <$> nodeExtent'
          Just (ExtentCoordinates ce) ->
            case parentNode of
              Just _ -> Just $ ExtentCoordinates $ CoordinateExtent
                (extentMinX ce + parentX) (extentMinY ce + parentY)
                (extentMaxX ce + parentX) (extentMaxY ce + parentY)
              Nothing -> Just (ExtentCoordinates ce)
          Just ExtentParent -> ExtentCoordinates <$> nodeExtent'
          Nothing -> ExtentCoordinates <$> nodeExtent'
      measuredDims = Dimensions
        (fromMaybe 0 (measuredWidth (internalMeasured n)))
        (fromMaybe 0 (measuredHeight (internalMeasured n)))
      positionAbsolute =
        case extent of
          Just (ExtentCoordinates ce) -> clampPosition nextPosition ce measuredDims
          _ -> nextPosition
      mw = fromMaybe 0 (measuredWidth (internalMeasured n))
      mh = fromMaybe 0 (measuredHeight (internalMeasured n))
  pure
    ( XYPosition
        (xyX positionAbsolute - parentX + mw * ox)
        (xyY positionAbsolute - parentY + mh * oy)
    , positionAbsolute
    )
-----------------------------------------------------------------------------
-- | Which of the requested nodes and edges may actually be deleted; port
-- of @getElementsToRemove@ (without the async @onBeforeDelete@ hook —
-- run your own check on the result instead).
getElementsToRemove
  :: [NodeId]
  -- ^ nodes to remove
  -> [EdgeId]
  -- ^ edges to remove
  -> [Node n]
  -- ^ all nodes
  -> [Edge e]
  -- ^ all edges
  -> ([Node n], [Edge e])
getElementsToRemove nodesToRemove edgesToRemove nodes edges =
  (matchingNodes, matchingEdges)
  where
    nodeIds = S.fromList nodesToRemove
    matchingNodes = foldl step [] nodes
      where
        step acc n
          | nodeDeletable n == Just False = acc
          | isIncluded || parentHit = acc <> [n]
          | otherwise = acc
          where
            isIncluded = nodeId n `S.member` nodeIds
            parentHit =
              not isIncluded &&
              case nodeParentId n of
                Nothing -> False
                Just pid -> any ((== pid) . nodeId) acc
    edgeIds = S.fromList edgesToRemove
    deletableEdges = [ e | e <- edges, edgeDeletable e /= Just False ]
    connectedEdges = getConnectedEdges matchingNodes deletableEdges
    matchingEdges =
      connectedEdges <>
      [ e | e <- deletableEdges
      , edgeId e `S.member` edgeIds
      , notElem (edgeId e) (map edgeId connectedEdges)
      ]
-----------------------------------------------------------------------------
