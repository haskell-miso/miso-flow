-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Store
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/store.ts@ from @\@xyflow\/system@: turning user
-- nodes into internal nodes ('adoptUserNodes'), keeping absolute
-- positions up to date, parent expansion, and the connection lookup.
--
-- The only part of the original left on the JavaScript side of the
-- bridge is @updateNodeInternals@' DOM measurement; its results are
-- merged back with 'applyMeasurement'.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Store
  ( UpdateNodesOptions (..)
  , defaultUpdateNodesOptions
  , AdoptUserNodesReturn (..)
  , adoptUserNodes
  , updateAbsolutePositions
  , isManualZIndexMode
  , ParentExpandChild (..)
  , handleExpandParent
  , updateConnectionLookup
  , applyMeasurement
  , NodeMeasurement (..)
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isNothing)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Flow.Constants (infiniteExtent)
import           Miso.Flow.Types
import           Miso.Flow.Utils.General
import           Miso.Flow.Utils.Graph (getNodePositionWithOrigin)
-----------------------------------------------------------------------------
selectedNodeZ :: Double
selectedNodeZ = 1000
-----------------------------------------------------------------------------
rootParentZIncrement :: Double
rootParentZIncrement = 10
-----------------------------------------------------------------------------
-- | Options shared by 'adoptUserNodes' and 'updateAbsolutePositions';
-- port of @UpdateNodesOptions@ (the @defaults@ field is a function here).
data UpdateNodesOptions n = UpdateNodesOptions
  { unoNodeOrigin           :: !NodeOrigin
  , unoNodeExtent           :: !CoordinateExtent
  , unoElevateNodesOnSelect :: !Bool
  , unoZIndexMode           :: !ZIndexMode
  , unoDefaults             :: Node n -> Node n
    -- ^ applied to each user node before adoption (TS spreads a partial
    -- node over it)
  , unoCheckEquality        :: !Bool
  }
-----------------------------------------------------------------------------
defaultUpdateNodesOptions :: UpdateNodesOptions n
defaultUpdateNodesOptions = UpdateNodesOptions
  { unoNodeOrigin = defaultOrigin
  , unoNodeExtent = infiniteExtent
  , unoElevateNodesOnSelect = True
  , unoZIndexMode = ZIndexBasic
  , unoDefaults = id
  , unoCheckEquality = True
  }
-----------------------------------------------------------------------------
data AdoptUserNodesReturn = AdoptUserNodesReturn
  { adoptNodesInitialized :: !Bool
  , adoptHasSelectedNodes :: !Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
isManualZIndexMode :: ZIndexMode -> Bool
isManualZIndexMode = (== ZIndexManual)
-----------------------------------------------------------------------------
calculateZ :: Node n -> Double -> ZIndexMode -> Double
calculateZ n selectedZ zIndexMode
  | isManualZIndexMode zIndexMode = zIndex
  | otherwise = zIndex + (if nodeSelected n then selectedZ else 0)
  where
    zIndex = case nodeZIndex n of
      Just z | isNumeric z -> z
      _ -> 0
-----------------------------------------------------------------------------
-- | Build the internal 'NodeLookup' \/ 'ParentLookup' from user nodes;
-- port of @adoptUserNodes@. The previous lookup preserves measured
-- dimensions and DOM-measured handle bounds across updates (standing in
-- for the TS reference-equality cache, using 'Eq' instead).
adoptUserNodes
  :: Eq n
  => [Node n]
  -> NodeLookup n
  -- ^ previous lookup (pass 'M.empty' initially)
  -> UpdateNodesOptions n
  -> (NodeLookup n, ParentLookup n, AdoptUserNodesReturn)
adoptUserNodes nodes prevLookup opts@UpdateNodesOptions {..} =
  finish (foldl step (M.empty, M.empty, initial, RootIndex 0) nodes)
  where
    initial = AdoptUserNodesReturn (not (null nodes)) False
    selectedZ =
      if unoElevateNodesOnSelect && not (isManualZIndexMode unoZIndexMode)
        then selectedNodeZ
        else 0
    finish (nl, pl, ret, _) = (nl, pl, ret)
    step (nodeLookup, parentLookup, ret, rootIndex) userNode0 =
      let userNode = unoDefaults userNode0
          prev = M.lookup (nodeId userNode) prevLookup
          internal = case prev of
            Just p
              | unoCheckEquality
              , internalUser p == userNode -> p
            _ ->
              let positionWithOrigin =
                    getNodePositionWithOrigin userNode unoNodeOrigin
                  extent = case nodeExtent userNode of
                    Just (ExtentCoordinates ce) -> ce
                    _ -> unoNodeExtent
                  clampedPosition = clampPosition
                    positionWithOrigin extent (getNodeDimensions userNode)
              in InternalNode
                { internalUser = userNode
                , internalMeasured = Measured
                    { measuredWidth = measuredWidth =<< nodeMeasured userNode
                    , measuredHeight = measuredHeight =<< nodeMeasured userNode
                    }
                , internalPositionAbsolute = clampedPosition
                , internalZ = calculateZ userNode selectedZ unoZIndexMode
                , internalRootParentIndex = Nothing
                , internalHandleBounds = parseHandles userNode prev
                , internalBounds = Nothing
                }
          uninitialized =
            (isNothing (measuredWidth (internalMeasured internal))
              || isNothing (measuredHeight (internalMeasured internal)))
            && not (nodeHidden userNode)
          ret' = AdoptUserNodesReturn
            { adoptNodesInitialized =
                adoptNodesInitialized ret && not uninitialized
            , adoptHasSelectedNodes =
                adoptHasSelectedNodes ret || nodeSelected userNode
            }
          nodeLookup' = M.insert (nodeId userNode) internal nodeLookup
          (nodeLookup'', parentLookup', rootIndex') =
            case nodeParentId userNode of
              Just _ ->
                updateChildNode internal nodeLookup' parentLookup opts (Just rootIndex)
              Nothing -> (nodeLookup', parentLookup, rootIndex)
      in (nodeLookup'', parentLookup', ret', rootIndex')
-----------------------------------------------------------------------------
-- | Port of @parseHandles@: use the node's declared handles when
-- present; otherwise keep previously measured bounds when the node
-- already carries measured dimensions.
parseHandles :: Node n -> Maybe (InternalNode n) -> Maybe NodeHandleBounds
parseHandles userNode prev =
  case nodeHandles userNode of
    Nothing
      | isNothing (nodeMeasured userNode) -> Nothing
      | otherwise -> internalHandleBounds =<< prev
    Just handles ->
      Just NodeHandleBounds
        { nhbSource = Just [ h | h <- hs, hType h == SourceHandle ]
        , nhbTarget = Just [ h | h <- hs, hType h == TargetHandle ]
        }
      where
        hs = map toHandle handles
        toHandle nh = Handle
          { hId = nhId nh
          , hNodeId = nodeId userNode
          , hX = nhX nh
          , hY = nhY nh
          , hPosition = nhPosition nh
          , hType = nhType nh
          , hWidth = fromMaybe 1 (nhWidth nh)
          , hHeight = fromMaybe 1 (nhHeight nh)
          }
-----------------------------------------------------------------------------
newtype RootIndex = RootIndex Int
-----------------------------------------------------------------------------
-- | Update @positionAbsolute@ and z-index of a child node and the parent
-- lookup; port of @updateChildNode@.
updateChildNode
  :: InternalNode n
  -> NodeLookup n
  -> ParentLookup n
  -> UpdateNodesOptions n
  -> Maybe RootIndex
  -> (NodeLookup n, ParentLookup n, RootIndex)
updateChildNode n nodeLookup parentLookup UpdateNodesOptions {..} mRootIndex =
  case M.lookup parentId nodeLookup of
    Nothing ->
      -- parent nodes must come before their children in the nodes array
      (nodeLookup, parentLookup, fromMaybe (RootIndex 0) mRootIndex)
    Just parentNode0 ->
      let parentLookup' = M.insertWith M.union parentId
            (M.singleton (nodeId (internalUser n)) n) parentLookup
          -- root parent z elevation (auto mode only)
          (parentNode, nodeLookup1, rootIndex1) =
            case mRootIndex of
              Just (RootIndex i)
                | isNothing (nodeParentId (internalUser parentNode0))
                , isNothing (internalRootParentIndex parentNode0)
                , unoZIndexMode == ZIndexAuto ->
                    let i' = i + 1
                        p' = parentNode0
                          { internalRootParentIndex = Just i'
                          , internalZ = internalZ parentNode0
                              + fromIntegral i' * rootParentZIncrement
                          }
                    in (p', M.insert parentId p' nodeLookup, RootIndex i')
              _ ->
                let idx = case internalRootParentIndex parentNode0 of
                      Just i -> RootIndex i
                      Nothing -> fromMaybe (RootIndex 0) mRootIndex
                in (parentNode0, nodeLookup, idx)
          selectedZ =
            if unoElevateNodesOnSelect && not (isManualZIndexMode unoZIndexMode)
              then selectedNodeZ
              else 0
          (x, y, z) = calculateChildXYZ n parentNode
            unoNodeOrigin unoNodeExtent selectedZ unoZIndexMode
          XYPosition ax ay = internalPositionAbsolute n
          positionChanged = x /= ax || y /= ay
          nodeLookup2
            | positionChanged || z /= internalZ n =
                M.insert (nodeId (internalUser n))
                  n { internalPositionAbsolute =
                        if positionChanged then XYPosition x y
                        else internalPositionAbsolute n
                    , internalZ = z
                    }
                  nodeLookup1
            | otherwise = nodeLookup1
      in (nodeLookup2, parentLookup', rootIndex1)
  where
    parentId = fromMaybe "" (nodeParentId (internalUser n))
-----------------------------------------------------------------------------
calculateChildXYZ
  :: InternalNode n
  -> InternalNode n
  -> NodeOrigin
  -> CoordinateExtent
  -> Double
  -> ZIndexMode
  -> (Double, Double, Double)
calculateChildXYZ childNode parentNode nodeOrigin' nodeExtent' selectedZ zIndexMode =
  (xyX absolutePosition', xyY absolutePosition', zOut)
  where
    child = internalUser childNode
    XYPosition parentX parentY = internalPositionAbsolute parentNode
    childDimensions = getInternalNodeDimensions childNode
    positionWithOrigin = getNodePositionWithOrigin child nodeOrigin'
    clampedPosition = case nodeExtent child of
      Just (ExtentCoordinates ce) ->
        clampPosition positionWithOrigin ce childDimensions
      _ -> positionWithOrigin
    absolutePosition = clampPosition
      (XYPosition (parentX + xyX clampedPosition) (parentY + xyY clampedPosition))
      nodeExtent'
      childDimensions
    absolutePosition' = case nodeExtent child of
      Just ExtentParent ->
        clampPositionToParent absolutePosition childDimensions parentNode
      _ -> absolutePosition
    childZ = calculateZ child selectedZ zIndexMode
    parentZ = internalZ parentNode
    zOut = if parentZ >= childZ then parentZ + 1 else childZ
-----------------------------------------------------------------------------
-- | Refresh @positionAbsolute@ of every node in the lookup; port of
-- @updateAbsolutePositions@.
updateAbsolutePositions
  :: NodeLookup n
  -> UpdateNodesOptions n
  -> NodeLookup n
updateAbsolutePositions nodeLookup opts@UpdateNodesOptions {..} =
  foldl step nodeLookup (M.elems nodeLookup)
  where
    step nl n =
      case nodeParentId (internalUser n) of
        Just _ ->
          let (nl', _, _) = updateChildNode n nl M.empty opts Nothing
          in nl'
        Nothing ->
          let u = internalUser n
              positionWithOrigin = getNodePositionWithOrigin u unoNodeOrigin
              extent = case nodeExtent u of
                Just (ExtentCoordinates ce) -> ce
                _ -> unoNodeExtent
              clampedPosition =
                clampPosition positionWithOrigin extent (getNodeDimensions u)
          in M.insert (nodeId u) n { internalPositionAbsolute = clampedPosition } nl
-----------------------------------------------------------------------------
-- | A child that may cause its parent to expand.
data ParentExpandChild = ParentExpandChild
  { pecId       :: !NodeId
  , pecParentId :: !NodeId
  , pecRect     :: !Rect
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Changes needed to expand parents so the given children fit; port of
-- @handleExpandParent@.
handleExpandParent
  :: [ParentExpandChild]
  -> NodeLookup n
  -> ParentLookup n
  -> NodeOrigin
  -> [NodeChange n]
handleExpandParent children nodeLookup parentLookup nodeOrigin' =
  concatMap expand (M.toList parentExpansions)
  where
    parentExpansions = foldl step M.empty children
      where
        step acc child =
          case M.lookup (pecParentId child) nodeLookup of
            Nothing -> acc
            Just parent ->
              let parentRect = case M.lookup (pecParentId child) acc of
                    Just (r, _) -> r
                    Nothing -> internalNodeToRect parent
                  expandedRect = getBoundsOfRects parentRect (pecRect child)
              in M.insert (pecParentId child) (expandedRect, parent) acc
    expand (parentId, (expandedRect, parent)) =
      let positionAbsolute = internalPositionAbsolute parent
          dimensions = getInternalNodeDimensions parent
          NodeOrigin ox oy =
            fromMaybe nodeOrigin' (nodeOrigin (internalUser parent))
          jsRound' v = fromIntegral (round v :: Integer)
          xChange =
            if rectX expandedRect < xyX positionAbsolute
              then jsRound' (abs (xyX positionAbsolute - rectX expandedRect))
              else 0
          yChange =
            if rectY expandedRect < xyY positionAbsolute
              then jsRound' (abs (xyY positionAbsolute - rectY expandedRect))
              else 0
          newWidth = max (dimensionsWidth dimensions) (jsRound' (rectWidth expandedRect))
          newHeight = max (dimensionsHeight dimensions) (jsRound' (rectHeight expandedRect))
          widthChange = (newWidth - dimensionsWidth dimensions) * ox
          heightChange = (newHeight - dimensionsHeight dimensions) * oy
          parentPos = nodePosition (internalUser parent)
          positionChanges
            | xChange > 0 || yChange > 0 || widthChange /= 0 || heightChange /= 0 =
                NodePositionChange parentId
                  (Just (XYPosition
                    (xyX parentPos - xChange + widthChange)
                    (xyY parentPos - yChange + heightChange)))
                  Nothing
                  Nothing
                : -- move children in the opposite direction so they stay put
                [ NodePositionChange (nodeId (internalUser childNode))
                    (Just (XYPosition
                      (xyX (nodePosition (internalUser childNode)) + xChange)
                      (xyY (nodePosition (internalUser childNode)) + yChange)))
                    Nothing
                    Nothing
                | childNode <- maybe [] M.elems (M.lookup parentId parentLookup)
                , notElem (nodeId (internalUser childNode)) (map pecId children)
                ]
            | otherwise = []
          dimensionChanges
            | dimensionsWidth dimensions < rectWidth expandedRect
                || dimensionsHeight dimensions < rectHeight expandedRect
                || xChange /= 0 || yChange /= 0 =
                [ NodeDimensionChange parentId
                    (Just (Dimensions
                      (newWidth + (if xChange /= 0 then ox * xChange - widthChange else 0))
                      (newHeight + (if yChange /= 0 then oy * yChange - heightChange else 0))))
                    Nothing
                    SetAttributesBoth
                ]
            | otherwise = []
      in positionChanges <> dimensionChanges
-----------------------------------------------------------------------------
-- | Build the 'ConnectionLookup' and 'EdgeLookup' from edges; port of
-- @updateConnectionLookup@.
updateConnectionLookup :: [Edge e] -> (ConnectionLookup, EdgeLookup e)
updateConnectionLookup edges =
  foldl step (M.empty, M.empty) edges
  where
    step (connectionLookup, edgeLookup) e =
      let sourceNode = edgeSource e
          targetNode = edgeTarget e
          sourceHandle = edgeSourceHandle e
          targetHandle = edgeTargetHandle e
          conn = HandleConnection
            { handleConnection = Connection
                { connectionSource = sourceNode
                , connectionTarget = targetNode
                , connectionSourceHandle = sourceHandle
                , connectionTargetHandle = targetHandle
                }
            , handleConnectionEdgeId = edgeId e
            }
          textOf = fromMaybe "null"
          sourceKey = mconcat
            [ sourceNode, "-", textOf sourceHandle
            , "--", targetNode, "-", textOf targetHandle ]
          targetKey = mconcat
            [ targetNode, "-", textOf targetHandle
            , "--", sourceNode, "-", textOf sourceHandle ]
          add ty nid mHandle key cl =
            let keys =
                  [ nid, nid <> "-" <> ty ]
                  <> [ nid <> "-" <> ty <> "-" <> h | Just h <- [mHandle] ]
            in foldl
                (\acc k -> M.insertWith M.union k (M.singleton key conn) acc)
                cl keys
          connectionLookup' =
            add "target" targetNode targetHandle sourceKey
              (add "source" sourceNode sourceHandle targetKey connectionLookup)
      in (connectionLookup', M.insert (edgeId e) e edgeLookup)
-----------------------------------------------------------------------------
-- | DOM measurement of one node, as reported over the bridge by the
-- JavaScript side's @updateNodeInternals@.
data NodeMeasurement = NodeMeasurement
  { nmId               :: !NodeId
  , nmDimensions       :: !Dimensions
  , nmHandleBounds     :: !(Maybe NodeHandleBounds)
  , nmPositionAbsolute :: !(Maybe XYPosition)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Merge measured dimensions and handle bounds into the lookup (the
-- Haskell half of @updateNodeInternals@).
applyMeasurement :: [NodeMeasurement] -> NodeLookup n -> NodeLookup n
applyMeasurement ms nodeLookup = foldl step nodeLookup ms
  where
    step nl NodeMeasurement {..} =
      case M.lookup nmId nl of
        Nothing -> nl
        Just n -> M.insert nmId
          n { internalMeasured = Measured
                (Just (dimensionsWidth nmDimensions))
                (Just (dimensionsHeight nmDimensions))
            , internalHandleBounds = case nmHandleBounds of
                Just hb -> Just hb
                Nothing -> internalHandleBounds n
            , internalPositionAbsolute =
                fromMaybe (internalPositionAbsolute n) nmPositionAbsolute
            }
          nl
-----------------------------------------------------------------------------
