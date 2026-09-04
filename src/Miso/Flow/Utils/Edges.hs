-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Edges
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/edges\/*@ from @\@xyflow\/system@: bezier, smooth
-- step and straight edge paths, edge centers, z-indexes, visibility,
-- 'addEdge' \/ 'reconnectEdge', and edge \/ handle position resolution.
--
-- All path-producing functions return SVG path strings identical (to the
-- byte) to the ones produced by the original TypeScript.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Edges
  ( -- * Path results
    EdgePath (..)
    -- * Bezier (@bezier-edge.ts@)
  , GetBezierPathParams (..)
  , bezierPathParams
  , getBezierPath
  , getBezierEdgeCenter
    -- * Simple bezier (from the framework packages)
  , getSimpleBezierPath
    -- * Smooth step (@smoothstep-edge.ts@)
  , GetSmoothStepPathParams (..)
  , smoothStepPathParams
  , getSmoothStepPath
    -- * Straight (@straight-edge.ts@)
  , getStraightPath
    -- * General (@edges\/general.ts@)
  , getEdgeCenter
  , getElevatedEdgeZIndex
  , isEdgeVisible
  , getEdgeId
  , connectionToEdge
  , addEdge
  , addEdgeWith
  , reconnectEdge
  , reconnectEdgeWith
    -- * Positions (@edges\/positions.ts@)
  , getEdgePosition
  , getHandlePosition
  , toHandleBounds
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Miso.Flow.Internal.JSNum (jsShow)
import           Miso.Flow.Types
import           Miso.Flow.Utils.General
-----------------------------------------------------------------------------
-- | Everything needed to render an edge: the SVG path plus the label
-- anchor. Port of the @[path, labelX, labelY, offsetX, offsetY]@ tuples.
data EdgePath = EdgePath
  { edgePath        :: !MisoString
    -- ^ value for the @d@ attribute of an SVG @\<path\>@
  , edgePathLabelX  :: !Double
  , edgePathLabelY  :: !Double
  , edgePathOffsetX :: !Double
  , edgePathOffsetY :: !Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data GetBezierPathParams = GetBezierPathParams
  { bezierSourceX        :: !Double
  , bezierSourceY        :: !Double
  , bezierSourcePosition :: !Position  -- ^ default 'PositionBottom'
  , bezierTargetX        :: !Double
  , bezierTargetY        :: !Double
  , bezierTargetPosition :: !Position  -- ^ default 'PositionTop'
  , bezierCurvatureParam :: !Double    -- ^ default 0.25
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Params with xyflow's defaults filled in.
bezierPathParams
  :: Double -- ^ source x
  -> Double -- ^ source y
  -> Double -- ^ target x
  -> Double -- ^ target y
  -> GetBezierPathParams
bezierPathParams sx sy tx ty = GetBezierPathParams
  { bezierSourceX = sx
  , bezierSourceY = sy
  , bezierSourcePosition = PositionBottom
  , bezierTargetX = tx
  , bezierTargetY = ty
  , bezierTargetPosition = PositionTop
  , bezierCurvatureParam = 0.25
  }
-----------------------------------------------------------------------------
-- | Center of a cubic bezier edge (t = 0.5 point) and offsets from the
-- source.
getBezierEdgeCenter
  :: Double -> Double  -- ^ source x, y
  -> Double -> Double  -- ^ target x, y
  -> Double -> Double  -- ^ source control x, y
  -> Double -> Double  -- ^ target control x, y
  -> (Double, Double, Double, Double)
getBezierEdgeCenter sourceX sourceY targetX targetY scX scY tcX tcY =
  ( centerX, centerY, abs (centerX - sourceX), abs (centerY - sourceY) )
  where
    centerX = sourceX * 0.125 + scX * 0.375 + tcX * 0.375 + targetX * 0.125
    centerY = sourceY * 0.125 + scY * 0.375 + tcY * 0.375 + targetY * 0.125
-----------------------------------------------------------------------------
calculateControlOffset :: Double -> Double -> Double
calculateControlOffset distance curvature
  | distance >= 0 = 0.5 * distance
  | otherwise = curvature * 25 * sqrt (negate distance)
-----------------------------------------------------------------------------
getControlWithCurvature
  :: Position -> Double -> Double -> Double -> Double -> Double
  -> (Double, Double)
getControlWithCurvature pos x1 y1 x2 y2 c =
  case pos of
    PositionLeft   -> (x1 - calculateControlOffset (x1 - x2) c, y1)
    PositionRight  -> (x1 + calculateControlOffset (x2 - x1) c, y1)
    PositionTop    -> (x1, y1 - calculateControlOffset (y1 - y2) c)
    PositionBottom -> (x1, y1 + calculateControlOffset (y2 - y1) c)
-----------------------------------------------------------------------------
-- | Bezier path between two points; port of @getBezierPath@.
getBezierPath :: GetBezierPathParams -> EdgePath
getBezierPath GetBezierPathParams {..} =
  EdgePath
    { edgePath = mconcat
        [ "M", jsShow bezierSourceX, ",", jsShow bezierSourceY
        , " C", jsShow scX, ",", jsShow scY
        , " ", jsShow tcX, ",", jsShow tcY
        , " ", jsShow bezierTargetX, ",", jsShow bezierTargetY
        ]
    , edgePathLabelX = labelX
    , edgePathLabelY = labelY
    , edgePathOffsetX = offsetX
    , edgePathOffsetY = offsetY
    }
  where
    (scX, scY) = getControlWithCurvature
      bezierSourcePosition bezierSourceX bezierSourceY
      bezierTargetX bezierTargetY bezierCurvatureParam
    (tcX, tcY) = getControlWithCurvature
      bezierTargetPosition bezierTargetX bezierTargetY
      bezierSourceX bezierSourceY bezierCurvatureParam
    (labelX, labelY, offsetX, offsetY) =
      getBezierEdgeCenter
        bezierSourceX bezierSourceY bezierTargetX bezierTargetY
        scX scY tcX tcY
-----------------------------------------------------------------------------
-- | Simple bezier path (control points halfway along the axis of the
-- handle position); port of @getSimpleBezierPath@ from the framework
-- packages.
getSimpleBezierPath
  :: Double -> Double -> Position  -- ^ source x, y, position
  -> Double -> Double -> Position  -- ^ target x, y, position
  -> EdgePath
getSimpleBezierPath sourceX sourceY sourcePosition targetX targetY targetPosition =
  EdgePath
    { edgePath = mconcat
        [ "M", jsShow sourceX, ",", jsShow sourceY
        , " C", jsShow scX, ",", jsShow scY
        , " ", jsShow tcX, ",", jsShow tcY
        , " ", jsShow targetX, ",", jsShow targetY
        ]
    , edgePathLabelX = labelX
    , edgePathLabelY = labelY
    , edgePathOffsetX = offsetX
    , edgePathOffsetY = offsetY
    }
  where
    control pos x1 y1 x2 y2 =
      case pos of
        PositionLeft   -> ((x1 + x2) / 2, y1)
        PositionRight  -> ((x1 + x2) / 2, y1)
        PositionTop    -> (x1, (y1 + y2) / 2)
        PositionBottom -> (x1, (y1 + y2) / 2)
    (scX, scY) = control sourcePosition sourceX sourceY targetX targetY
    (tcX, tcY) = control targetPosition targetX targetY sourceX sourceY
    (labelX, labelY, offsetX, offsetY) =
      getBezierEdgeCenter sourceX sourceY targetX targetY scX scY tcX tcY
-----------------------------------------------------------------------------
data GetSmoothStepPathParams = GetSmoothStepPathParams
  { smoothSourceX        :: !Double
  , smoothSourceY        :: !Double
  , smoothSourcePosition :: !Position       -- ^ default 'PositionBottom'
  , smoothTargetX        :: !Double
  , smoothTargetY        :: !Double
  , smoothTargetPosition :: !Position       -- ^ default 'PositionTop'
  , smoothBorderRadius   :: !Double         -- ^ default 5
  , smoothCenterX        :: !(Maybe Double)
  , smoothCenterY        :: !(Maybe Double)
  , smoothOffset         :: !Double         -- ^ default 20
  , smoothStepPosition'  :: !Double         -- ^ default 0.5
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Params with xyflow's defaults filled in.
smoothStepPathParams
  :: Double -- ^ source x
  -> Double -- ^ source y
  -> Double -- ^ target x
  -> Double -- ^ target y
  -> GetSmoothStepPathParams
smoothStepPathParams sx sy tx ty = GetSmoothStepPathParams
  { smoothSourceX = sx
  , smoothSourceY = sy
  , smoothSourcePosition = PositionBottom
  , smoothTargetX = tx
  , smoothTargetY = ty
  , smoothTargetPosition = PositionTop
  , smoothBorderRadius = 5
  , smoothCenterX = Nothing
  , smoothCenterY = Nothing
  , smoothOffset = 20
  , smoothStepPosition' = 0.5
  }
-----------------------------------------------------------------------------
handleDirection :: Position -> XYPosition
handleDirection = \case
  PositionLeft   -> XYPosition (-1) 0
  PositionRight  -> XYPosition 1 0
  PositionTop    -> XYPosition 0 (-1)
  PositionBottom -> XYPosition 0 1
-----------------------------------------------------------------------------
getDirection :: XYPosition -> Position -> XYPosition -> XYPosition
getDirection source sourcePosition target
  | sourcePosition == PositionLeft || sourcePosition == PositionRight =
      if xyX source < xyX target then XYPosition 1 0 else XYPosition (-1) 0
  | otherwise =
      if xyY source < xyY target then XYPosition 0 1 else XYPosition 0 (-1)
-----------------------------------------------------------------------------
distanceBetween :: XYPosition -> XYPosition -> Double
distanceBetween a b =
  sqrt ((xyX b - xyX a) ** 2 + (xyY b - xyY a) ** 2)
-----------------------------------------------------------------------------
data Axis = AxisX | AxisY deriving (Eq)
-----------------------------------------------------------------------------
axis :: Axis -> XYPosition -> Double
axis AxisX = xyX
axis AxisY = xyY
-----------------------------------------------------------------------------
setAxis :: Axis -> Double -> XYPosition -> XYPosition
setAxis AxisX v p = p { xyX = v }
setAxis AxisY v p = p { xyY = v }
-----------------------------------------------------------------------------
-- | Orthogonal routing points for step edges; port of @getPoints@.
getPoints
  :: XYPosition -> Position   -- ^ source, source position
  -> XYPosition -> Position   -- ^ target, target position
  -> (Maybe Double, Maybe Double)  -- ^ center override
  -> Double                   -- ^ offset
  -> Double                   -- ^ step position
  -> ([XYPosition], Double, Double, Double, Double)
getPoints source sourcePosition target targetPosition (centerX', centerY') offset stepPosition =
  (pathPoints, centerX, centerY, defaultOffsetX, defaultOffsetY)
  where
    sourceDir = handleDirection sourcePosition
    targetDir = handleDirection targetPosition
    sourceGapped = XYPosition (xyX source + xyX sourceDir * offset)
                              (xyY source + xyY sourceDir * offset)
    targetGapped = XYPosition (xyX target + xyX targetDir * offset)
                              (xyY target + xyY targetDir * offset)
    dir = getDirection sourceGapped sourcePosition targetGapped
    dirAccessor = if xyX dir /= 0 then AxisX else AxisY
    currDir = axis dirAccessor dir

    (_, _, defaultOffsetX, defaultOffsetY) =
      getEdgeCenter (xyX source) (xyY source) (xyX target) (xyY target)

    oppositeHandles = axis dirAccessor sourceDir * axis dirAccessor targetDir == -1

    (points, centerX, centerY, sourceGapOffset, targetGapOffset)
      | oppositeHandles =
          let cX = case dirAccessor of
                AxisX -> fromMaybe
                  (xyX sourceGapped + (xyX targetGapped - xyX sourceGapped) * stepPosition)
                  centerX'
                AxisY -> fromMaybe
                  ((xyX sourceGapped + xyX targetGapped) / 2) centerX'
              cY = case dirAccessor of
                AxisX -> fromMaybe
                  ((xyY sourceGapped + xyY targetGapped) / 2) centerY'
                AxisY -> fromMaybe
                  (xyY sourceGapped + (xyY targetGapped - xyY sourceGapped) * stepPosition)
                  centerY'
              verticalSplit =
                [ XYPosition cX (xyY sourceGapped)
                , XYPosition cX (xyY targetGapped) ]
              horizontalSplit =
                [ XYPosition (xyX sourceGapped) cY
                , XYPosition (xyX targetGapped) cY ]
              ps | axis dirAccessor sourceDir == currDir =
                     if dirAccessor == AxisX then verticalSplit else horizontalSplit
                 | otherwise =
                     if dirAccessor == AxisX then horizontalSplit else verticalSplit
          in (ps, cX, cY, zeroPosition, zeroPosition)
      | otherwise =
          let sourceTarget = [ XYPosition (xyX sourceGapped) (xyY targetGapped) ]
              targetSource = [ XYPosition (xyX targetGapped) (xyY sourceGapped) ]
              ps0 | dirAccessor == AxisX =
                      if xyX sourceDir == currDir then targetSource else sourceTarget
                  | otherwise =
                      if xyY sourceDir == currDir then sourceTarget else targetSource
              -- same handle positions: keep gapped points from overlapping
              (sourceGapOff, targetGapOff)
                | sourcePosition == targetPosition =
                    let diff = abs (axis dirAccessor source - axis dirAccessor target)
                    in if diff <= offset
                         then
                           let gapOffset = min (offset - 1) (offset - diff)
                           in if axis dirAccessor sourceDir == currDir
                                then ( setAxis dirAccessor
                                         ((if axis dirAccessor sourceGapped > axis dirAccessor source then -1 else 1) * gapOffset)
                                         zeroPosition
                                     , zeroPosition )
                                else ( zeroPosition
                                     , setAxis dirAccessor
                                         ((if axis dirAccessor targetGapped > axis dirAccessor target then -1 else 1) * gapOffset)
                                         zeroPosition )
                         else (zeroPosition, zeroPosition)
                | otherwise = (zeroPosition, zeroPosition)
              -- mixed handle positions (e.g. Right -> Bottom)
              ps | sourcePosition /= targetPosition =
                    let dirAccessorOpposite = if dirAccessor == AxisX then AxisY else AxisX
                        isSameDir = axis dirAccessor sourceDir == axis dirAccessorOpposite targetDir
                        sourceGtTargetOppo =
                          axis dirAccessorOpposite sourceGapped > axis dirAccessorOpposite targetGapped
                        sourceLtTargetOppo =
                          axis dirAccessorOpposite sourceGapped < axis dirAccessorOpposite targetGapped
                        flipSourceTarget =
                          (axis dirAccessor sourceDir == 1 &&
                            ((not isSameDir && sourceGtTargetOppo) || (isSameDir && sourceLtTargetOppo)))
                          ||
                          (axis dirAccessor sourceDir /= 1 &&
                            ((not isSameDir && sourceLtTargetOppo) || (isSameDir && sourceGtTargetOppo)))
                    in if flipSourceTarget
                         then if dirAccessor == AxisX then sourceTarget else targetSource
                         else ps0
                 | otherwise = ps0
              sourceGapPoint = XYPosition (xyX sourceGapped + xyX sourceGapOff)
                                          (xyY sourceGapped + xyY sourceGapOff)
              targetGapPoint = XYPosition (xyX targetGapped + xyX targetGapOff)
                                          (xyY targetGapped + xyY targetGapOff)
              p0 = head ps
              maxXDistance = max (abs (xyX sourceGapPoint - xyX p0))
                                 (abs (xyX targetGapPoint - xyX p0))
              maxYDistance = max (abs (xyY sourceGapPoint - xyY p0))
                                 (abs (xyY targetGapPoint - xyY p0))
              (cX, cY)
                | maxXDistance >= maxYDistance =
                    ((xyX sourceGapPoint + xyX targetGapPoint) / 2, xyY p0)
                | otherwise =
                    (xyX p0, (xyY sourceGapPoint + xyY targetGapPoint) / 2)
          in (ps, cX, cY, sourceGapOff, targetGapOff)

    gappedSource = XYPosition (xyX sourceGapped + xyX sourceGapOffset)
                              (xyY sourceGapped + xyY sourceGapOffset)
    gappedTarget = XYPosition (xyX targetGapped + xyX targetGapOffset)
                              (xyY targetGapped + xyY targetGapOffset)
    firstPoint = head points
    lastPoint = last points
    pathPoints = mconcat
      [ [ source ]
      , [ gappedSource | gappedSource /= firstPoint ]
      , points
      , [ gappedTarget | gappedTarget /= lastPoint ]
      , [ target ]
      ]
-----------------------------------------------------------------------------
-- | Rounded bend between three points; port of @getBend@.
getBend :: XYPosition -> XYPosition -> XYPosition -> Double -> MisoString
getBend a b c size
  -- no bend
  | (xyX a == x && x == xyX c) || (xyY a == y && y == xyY c) =
      "L" <> jsShow x <> " " <> jsShow y
  -- first segment is horizontal
  | xyY a == y =
      let xDir = if xyX a < xyX c then -1 else 1
          yDir = if xyY a < xyY c then 1 else -1
      in mconcat
        [ "L ", jsShow (x + bendSize * xDir), ",", jsShow y
        , "Q ", jsShow x, ",", jsShow y
        , " ", jsShow x, ",", jsShow (y + bendSize * yDir)
        ]
  | otherwise =
      let xDir = if xyX a < xyX c then 1 else -1
          yDir = if xyY a < xyY c then -1 else 1
      in mconcat
        [ "L ", jsShow x, ",", jsShow (y + bendSize * yDir)
        , "Q ", jsShow x, ",", jsShow y
        , " ", jsShow (x + bendSize * xDir), ",", jsShow y
        ]
  where
    bendSize = minimum [ distanceBetween a b / 2, distanceBetween b c / 2, size ]
    x = xyX b
    y = xyY b
-----------------------------------------------------------------------------
-- | Stepped path between two points with rounded corners; port of
-- @getSmoothStepPath@. Use @smoothBorderRadius = 0@ for a plain step
-- edge.
getSmoothStepPath :: GetSmoothStepPathParams -> EdgePath
getSmoothStepPath GetSmoothStepPathParams {..} =
  EdgePath
    { edgePath = path
    , edgePathLabelX = labelX
    , edgePathLabelY = labelY
    , edgePathOffsetX = offsetX
    , edgePathOffsetY = offsetY
    }
  where
    (points, labelX, labelY, offsetX, offsetY) =
      getPoints
        (XYPosition smoothSourceX smoothSourceY) smoothSourcePosition
        (XYPosition smoothTargetX smoothTargetY) smoothTargetPosition
        (smoothCenterX, smoothCenterY)
        smoothOffset
        smoothStepPosition'
    firstPoint = head points
    lastPoint = last points
    bends = mconcat
      [ getBend (points !! (i - 1)) (points !! i) (points !! (i + 1)) smoothBorderRadius
      | i <- [1 .. length points - 2]
      ]
    path = mconcat
      [ "M", jsShow (xyX firstPoint), " ", jsShow (xyY firstPoint)
      , bends
      , "L", jsShow (xyX lastPoint), " ", jsShow (xyY lastPoint)
      ]
-----------------------------------------------------------------------------
-- | Straight line between two points; port of @getStraightPath@.
getStraightPath
  :: Double -- ^ source x
  -> Double -- ^ source y
  -> Double -- ^ target x
  -> Double -- ^ target y
  -> EdgePath
getStraightPath sourceX sourceY targetX targetY =
  EdgePath
    { edgePath = mconcat
        [ "M ", jsShow sourceX, ",", jsShow sourceY
        , "L ", jsShow targetX, ",", jsShow targetY
        ]
    , edgePathLabelX = labelX
    , edgePathLabelY = labelY
    , edgePathOffsetX = offsetX
    , edgePathOffsetY = offsetY
    }
  where
    (labelX, labelY, offsetX, offsetY) =
      getEdgeCenter sourceX sourceY targetX targetY
-----------------------------------------------------------------------------
-- | Center point of a straight edge and the offsets from the source;
-- returns @(centerX, centerY, offsetX, offsetY)@.
getEdgeCenter
  :: Double -> Double -> Double -> Double
  -> (Double, Double, Double, Double)
getEdgeCenter sourceX sourceY targetX targetY =
  (centerX, centerY, xOffset, yOffset)
  where
    xOffset = abs (targetX - sourceX) / 2
    centerX = if targetX < sourceX then targetX + xOffset else targetX - xOffset
    yOffset = abs (targetY - sourceY) / 2
    centerY = if targetY < sourceY then targetY + yOffset else targetY - yOffset
-----------------------------------------------------------------------------
-- | Z-index for an edge based on its endpoint nodes and selection state;
-- port of @getElevatedEdgeZIndex@.
getElevatedEdgeZIndex
  :: InternalNode n  -- ^ source node
  -> InternalNode n  -- ^ target node
  -> Bool            -- ^ selected
  -> Double          -- ^ z-index (default 0)
  -> Bool            -- ^ elevate on select
  -> ZIndexMode      -- ^ default 'ZIndexBasic'
  -> Double
getElevatedEdgeZIndex sourceNode targetNode selected zIndex elevateOnSelect zIndexMode
  | zIndexMode == ZIndexManual = zIndex
  | otherwise = edgeZ + nodeZ
  where
    edgeZ = if elevateOnSelect && selected then zIndex + 1000 else zIndex
    nodeContrib n =
      if isJust (nodeParentId (internalUser n))
           || (elevateOnSelect && nodeSelected (internalUser n))
        then internalZ n
        else 0
    nodeZ = max (nodeContrib sourceNode) (nodeContrib targetNode)
-----------------------------------------------------------------------------
-- | Whether any part of the edge's bounding box is inside the viewport.
isEdgeVisible
  :: InternalNode n  -- ^ source node
  -> InternalNode n  -- ^ target node
  -> Double          -- ^ viewport width
  -> Double          -- ^ viewport height
  -> Transform
  -> Bool
isEdgeVisible sourceNode targetNode width height (Viewport tx ty tScale) =
  getOverlappingArea viewRect (boxToRect edgeBox') > 0
  where
    edgeBox = getBoundsOfBoxes
      (internalNodeToBox sourceNode)
      (internalNodeToBox targetNode)
    edgeBox' = edgeBox
      { boxX2 = if boxX edgeBox == boxX2 edgeBox then boxX2 edgeBox + 1 else boxX2 edgeBox
      , boxY2 = if boxY edgeBox == boxY2 edgeBox then boxY2 edgeBox + 1 else boxY2 edgeBox
      }
    viewRect = Rect
      { rectX = negate tx / tScale
      , rectY = negate ty / tScale
      , rectWidth = width / tScale
      , rectHeight = height / tScale
      }
-----------------------------------------------------------------------------
-- | Default edge id for a connection:
-- @xy-edge__\<source>\<sourceHandle>-\<target>\<targetHandle>@.
getEdgeId :: Connection -> EdgeId
getEdgeId Connection {..} = mconcat
  [ "xy-edge__"
  , connectionSource, fromMaybe "" connectionSourceHandle
  , "-"
  , connectionTarget, fromMaybe "" connectionTargetHandle
  ]
-----------------------------------------------------------------------------
-- | Upgrade a 'Connection' to an 'Edge' using the given id generator.
connectionToEdge :: (Connection -> EdgeId) -> Connection -> Edge e
connectionToEdge mkId c =
  (edge (mkId c) (connectionSource c) (connectionTarget c))
    { edgeSourceHandle = connectionSourceHandle c
    , edgeTargetHandle = connectionTargetHandle c
    }
-----------------------------------------------------------------------------
connectionExists :: Edge e -> [Edge e] -> Bool
connectionExists e = any $ \el ->
  edgeSource el == edgeSource e
    && edgeTarget el == edgeTarget e
    && (edgeSourceHandle el == edgeSourceHandle e
          || (edgeSourceHandle el == Nothing && edgeSourceHandle e == Nothing))
    && (edgeTargetHandle el == edgeTargetHandle e
          || (edgeTargetHandle el == Nothing && edgeTargetHandle e == Nothing))
-----------------------------------------------------------------------------
-- | Add a connection to an array of edges, unless an equivalent
-- connection already exists; port of @addEdge@ (for the @Connection@
-- overload — pass an existing 'Edge' through 'addEdgeWith' 'id').
addEdge :: Connection -> [Edge e] -> [Edge e]
addEdge = addEdgeWith (connectionToEdge getEdgeId)
-----------------------------------------------------------------------------
-- | 'addEdge' with a custom @Connection -> Edge@ upgrade (covers the
-- custom @getEdgeId@ option and the edge overload of the original).
addEdgeWith :: (Connection -> Edge e) -> Connection -> [Edge e] -> [Edge e]
addEdgeWith mkEdge c edges
  | connectionSource c == "" || connectionTarget c == "" = edges
  | connectionExists e edges = edges
  | otherwise = edges <> [e]
  where
    e = mkEdge c
-----------------------------------------------------------------------------
-- | Replace an edge with a new connection; port of @reconnectEdge@ with
-- @shouldReplaceId = True@.
reconnectEdge :: Edge e -> Connection -> [Edge e] -> [Edge e]
reconnectEdge = reconnectEdgeWith True getEdgeId
-----------------------------------------------------------------------------
-- | 'reconnectEdge' with explicit @shouldReplaceId@ and id generator.
reconnectEdgeWith
  :: Bool
  -- ^ replace the old id with the generated connection id
  -> (Connection -> EdgeId)
  -> Edge e
  -> Connection
  -> [Edge e]
  -> [Edge e]
reconnectEdgeWith shouldReplaceId mkId oldEdge newConnection edges
  | connectionSource newConnection == ""
      || connectionTarget newConnection == "" = edges
  | not (any ((== edgeId oldEdge) . edgeId) edges) = edges
  | otherwise =
      [ e | e <- edges, edgeId e /= edgeId oldEdge ] <> [ newEdge ]
  where
    newEdge = oldEdge
      { edgeId = if shouldReplaceId then mkId newConnection else edgeId oldEdge
      , edgeSource = connectionSource newConnection
      , edgeTarget = connectionTarget newConnection
      , edgeSourceHandle = connectionSourceHandle newConnection
      , edgeTargetHandle = connectionTargetHandle newConnection
      , edgeSelected = False
      }
-----------------------------------------------------------------------------
isNodeInitialized :: InternalNode n -> Bool
isNodeInitialized n =
  (isJust (internalHandleBounds n)
     || maybe False (not . null) (nodeHandles u))
  && (isJust (measuredWidth (internalMeasured n))
       || isJust (nodeWidth u)
       || isJust (nodeInitialWidth u))
  where u = internalUser n
-----------------------------------------------------------------------------
-- | Resolve the concrete start\/end coordinates and positions of an edge
-- from its endpoint nodes; port of @getEdgePosition@. Returns 'Nothing'
-- when either node is not yet initialized or a handle can't be found.
getEdgePosition
  :: InternalNode n        -- ^ source node
  -> Maybe MisoString      -- ^ source handle id
  -> InternalNode n        -- ^ target node
  -> Maybe MisoString      -- ^ target handle id
  -> ConnectionMode
  -> Maybe EdgePosition
getEdgePosition sourceNode sourceHandleId targetNode targetHandleId connectionMode
  | not (isNodeInitialized sourceNode) || not (isNodeInitialized targetNode) =
      Nothing
  | otherwise = do
      let sourceBounds = handleBoundsOf sourceNode
          targetBounds = handleBoundsOf targetNode
      sourceHandle <- pickHandle
        (fromMaybe [] (nhbSource =<< sourceBounds)) sourceHandleId
      targetHandle <- pickHandle
        (case connectionMode of
           ConnectionModeStrict ->
             fromMaybe [] (nhbTarget =<< targetBounds)
           ConnectionModeLoose ->
             fromMaybe [] (nhbTarget =<< targetBounds)
               <> fromMaybe [] (nhbSource =<< targetBounds))
        targetHandleId
      let sourcePosition = hPosition sourceHandle
          targetPosition = hPosition targetHandle
          source = getHandlePosition sourceNode (Just sourceHandle) sourcePosition False
          target = getHandlePosition targetNode (Just targetHandle) targetPosition False
      pure EdgePosition
        { epSourceX = xyX source
        , epSourceY = xyY source
        , epTargetX = xyX target
        , epTargetY = xyY target
        , epSourcePosition = sourcePosition
        , epTargetPosition = targetPosition
        }
  where
    handleBoundsOf n =
      case internalHandleBounds n of
        Just hb -> Just hb
        Nothing -> toHandleBounds (nodeId (internalUser n)) (nodeHandles (internalUser n))
    pickHandle bounds handleId' =
      case handleId' of
        Nothing -> case bounds of
          (h : _) -> Just h
          [] -> Nothing
        Just hid -> case filter ((== Just hid) . hId) bounds of
          (h : _) -> Just h
          [] -> Nothing
-----------------------------------------------------------------------------
-- | Group a node's declared handles by type; port of @toHandleBounds@.
toHandleBounds :: NodeId -> Maybe [NodeHandle] -> Maybe NodeHandleBounds
toHandleBounds _ Nothing = Nothing
toHandleBounds nid (Just handles) =
  Just NodeHandleBounds
    { nhbSource = Just [ h | h <- hs, hType h == SourceHandle ]
    , nhbTarget = Just [ h | h <- hs, hType h == TargetHandle ]
    }
  where
    hs = map toHandle handles
    toHandle nh = Handle
      { hId = nhId nh
      , hNodeId = nid
      , hX = nhX nh
      , hY = nhY nh
      , hPosition = nhPosition nh
      , hType = nhType nh
      , hWidth = fromMaybe 1 (nhWidth nh)
      , hHeight = fromMaybe 1 (nhHeight nh)
      }
-----------------------------------------------------------------------------
-- | Absolute position of a handle on a node; port of @getHandlePosition@.
getHandlePosition
  :: InternalNode n
  -> Maybe Handle
  -> Position
  -- ^ fallback position (original default: 'PositionLeft')
  -> Bool
  -- ^ center: return the handle center instead of its edge anchor
  -> XYPosition
getHandlePosition n mHandle fallbackPosition center =
  if center
    then XYPosition (x + w / 2) (y + h / 2)
    else case position of
      PositionTop    -> XYPosition (x + w / 2) y
      PositionRight  -> XYPosition (x + w) (y + h / 2)
      PositionBottom -> XYPosition (x + w / 2) (y + h)
      PositionLeft   -> XYPosition x (y + h / 2)
  where
    x = maybe 0 hX mHandle + xyX (internalPositionAbsolute n)
    y = maybe 0 hY mHandle + xyY (internalPositionAbsolute n)
    Dimensions w h = case mHandle of
      Just handle -> Dimensions (hWidth handle) (hHeight handle)
      Nothing -> getInternalNodeDimensions n
    position = maybe fallbackPosition hPosition mHandle
-----------------------------------------------------------------------------
