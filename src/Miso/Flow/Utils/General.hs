-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.General
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/general.ts@ from @\@xyflow\/system@: clamping,
-- box\/rect algebra, viewport math and @getViewportForBounds@.
----------------------------------------------------------------------------
module Miso.Flow.Utils.General
  ( clamp
  , clampPosition
  , clampPositionToParent
  , calcAutoPanVelocity
  , calcAutoPan
  , getBoundsOfBoxes
  , rectToBox
  , boxToRect
  , nodeToRect
  , internalNodeToRect
  , nodeToBox
  , internalNodeToBox
  , getBoundsOfRects
  , getRectsOverlappingArea
  , getOverlappingArea
  , isNumeric
  , snapPosition
  , pointToRendererPoint
  , rendererPointToPoint
  , parsePadding
  , ParsedPaddings (..)
  , parsePaddings
  , getViewportForBounds
  , getNodeDimensions
  , getInternalNodeDimensions
  , nodeHasDimensions
  , evaluateAbsolutePosition
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | @clamp val min max@.
clamp :: Double -> Double -> Double -> Double
clamp val lo hi = min (max val lo) hi
-----------------------------------------------------------------------------
-- | Clamp a position into a 'CoordinateExtent', accounting for the
-- element's dimensions.
clampPosition :: XYPosition -> CoordinateExtent -> Dimensions -> XYPosition
clampPosition (XYPosition x y) CoordinateExtent {..} (Dimensions w h) =
  XYPosition
    (clamp x extentMinX (extentMaxX - w))
    (clamp y extentMinY (extentMaxY - h))
-----------------------------------------------------------------------------
-- | Clamp a child position into the bounds of its parent node.
clampPositionToParent
  :: XYPosition
  -> Dimensions
  -> InternalNode n
  -> XYPosition
clampPositionToParent childPosition childDimensions parent =
  let Dimensions pw ph = getInternalNodeDimensions parent
      XYPosition px py = internalPositionAbsolute parent
  in clampPosition childPosition
       (CoordinateExtent px py (px + pw) (py + ph))
       childDimensions
-----------------------------------------------------------------------------
-- | Velocity (-1..1) of auto panning when the pointer is close to a pane
-- edge. @calcAutoPanVelocity value min max@.
calcAutoPanVelocity :: Double -> Double -> Double -> Double
calcAutoPanVelocity value lo hi
  | value < lo = clamp (abs (value - lo)) 1 lo / lo
  | value > hi = negate (clamp (abs (value - hi)) 1 lo) / lo
  | otherwise = 0
-----------------------------------------------------------------------------
-- | X\/Y auto-pan movement for the given pointer position and pane bounds.
-- Speed defaults to 15, edge distance to 40 in the original.
calcAutoPan
  :: XYPosition
  -- ^ pointer position
  -> Dimensions
  -- ^ pane bounds
  -> Double
  -- ^ speed (original default: 15)
  -> Double
  -- ^ distance (original default: 40)
  -> (Double, Double)
calcAutoPan (XYPosition x y) (Dimensions w h) speed distance =
  ( calcAutoPanVelocity x distance (w - distance) * speed
  , calcAutoPanVelocity y distance (h - distance) * speed
  )
-----------------------------------------------------------------------------
getBoundsOfBoxes :: Box -> Box -> Box
getBoundsOfBoxes b1 b2 = Box
  { boxX  = min (boxX b1) (boxX b2)
  , boxY  = min (boxY b1) (boxY b2)
  , boxX2 = max (boxX2 b1) (boxX2 b2)
  , boxY2 = max (boxY2 b1) (boxY2 b2)
  }
-----------------------------------------------------------------------------
rectToBox :: Rect -> Box
rectToBox (Rect x y w h) = Box x y (x + w) (y + h)
-----------------------------------------------------------------------------
boxToRect :: Box -> Rect
boxToRect (Box x y x2 y2) = Rect x y (x2 - x) (y2 - y)
-----------------------------------------------------------------------------
-- | Bounding rect of a user node (positioned by origin). Port of
-- @nodeToRect@ for the non-internal case.
nodeToRect :: Node n -> NodeOrigin -> Rect
nodeToRect n nodeOrigin' =
  let XYPosition x y = getNodePositionWithOriginLocal n nodeOrigin'
      Dimensions w h = getNodeDimensions n
  in Rect x y w h
-----------------------------------------------------------------------------
-- | Bounding rect of an internal node (uses @positionAbsolute@). Port of
-- @nodeToRect@ for the internal case.
internalNodeToRect :: InternalNode n -> Rect
internalNodeToRect n =
  let XYPosition x y = internalPositionAbsolute n
      Dimensions w h = getInternalNodeDimensions n
  in Rect x y w h
-----------------------------------------------------------------------------
nodeToBox :: Node n -> NodeOrigin -> Box
nodeToBox n o = rectToBox (nodeToRect n o)
-----------------------------------------------------------------------------
internalNodeToBox :: InternalNode n -> Box
internalNodeToBox = rectToBox . internalNodeToRect
-----------------------------------------------------------------------------
getBoundsOfRects :: Rect -> Rect -> Rect
getBoundsOfRects r1 r2 = boxToRect (getBoundsOfBoxes (rectToBox r1) (rectToBox r2))
-----------------------------------------------------------------------------
getRectsOverlappingArea
  :: Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> Double
getRectsOverlappingArea aX aY aWidth aHeight bX bY bWidth bHeight =
  let xOverlap = max 0 (min (aX + aWidth) (bX + bWidth) - max aX bX)
      yOverlap = max 0 (min (aY + aHeight) (bY + bHeight) - max aY bY)
  in fromIntegral (ceiling (xOverlap * yOverlap) :: Integer)
-----------------------------------------------------------------------------
getOverlappingArea :: Rect -> Rect -> Double
getOverlappingArea (Rect ax ay aw ah) (Rect bx by bw bh) =
  getRectsOverlappingArea ax ay aw ah bx by bw bh
-----------------------------------------------------------------------------
-- | JS @isNumeric@: finite and not NaN.
isNumeric :: Double -> Bool
isNumeric n = not (isNaN n) && not (isInfinite n)
-----------------------------------------------------------------------------
snapPosition :: XYPosition -> SnapGrid -> XYPosition
snapPosition (XYPosition x y) (SnapGrid gx gy) = XYPosition
  (gx * jsRound (x / gx))
  (gy * jsRound (y / gy))
-----------------------------------------------------------------------------
-- | @Math.round@: half-up (towards +Infinity), unlike Haskell's
-- banker's rounding.
jsRound :: Double -> Double
jsRound v = fromIntegral (floor (v + 0.5) :: Integer)
-----------------------------------------------------------------------------
-- | Screen point → flow point, optionally snapped to grid.
pointToRendererPoint
  :: XYPosition
  -> Transform
  -> Bool
  -- ^ snap to grid
  -> SnapGrid
  -> XYPosition
pointToRendererPoint (XYPosition x y) (Viewport tx ty tScale) snapToGrid snapGrid =
  let position = XYPosition ((x - tx) / tScale) ((y - ty) / tScale)
  in if snapToGrid then snapPosition position snapGrid else position
-----------------------------------------------------------------------------
-- | Flow point → screen point.
rendererPointToPoint :: XYPosition -> Transform -> XYPosition
rendererPointToPoint (XYPosition x y) (Viewport tx ty tScale) =
  XYPosition (x * tScale + tx) (y * tScale + ty)
-----------------------------------------------------------------------------
-- | Resolve a single padding value to pixels; @parsePadding padding viewport@
-- where @viewport@ is the relevant viewport dimension.
parsePadding :: PaddingWithUnit -> Double -> Double
parsePadding p viewport =
  case p of
    PaddingRatio r ->
      fromIntegral (floor ((viewport - viewport / (1 + r)) * 0.5) :: Integer)
    PaddingPx v -> fromIntegral (floor v :: Integer)
    PaddingPercent v -> fromIntegral (floor (viewport * v * 0.01) :: Integer)
-----------------------------------------------------------------------------
data ParsedPaddings = ParsedPaddings
  { ppTop    :: !Double
  , ppRight  :: !Double
  , ppBottom :: !Double
  , ppLeft   :: !Double
  , ppX      :: !Double
  , ppY      :: !Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Resolve a 'Padding' to per-side pixel values for the given viewport
-- width and height.
parsePaddings :: Padding -> Double -> Double -> ParsedPaddings
parsePaddings padding width height =
  case padding of
    PaddingUniform p ->
      let py = parsePadding p height
          px = parsePadding p width
      in ParsedPaddings py px py px (px * 2) (py * 2)
    PaddingSides {..} ->
      let zero = PaddingRatio 0
          top = parsePadding (fromMaybe (fromMaybe zero paddingY) paddingTop) height
          bottom = parsePadding (fromMaybe (fromMaybe zero paddingY) paddingBottom) height
          left = parsePadding (fromMaybe (fromMaybe zero paddingX) paddingLeft) width
          right = parsePadding (fromMaybe (fromMaybe zero paddingX) paddingRight) width
      in ParsedPaddings top right bottom left (left + right) (top + bottom)
-----------------------------------------------------------------------------
-- | Minimum padding that remains around @bounds@ if the viewport
-- @(x, y, zoom)@ is applied; port of @calculateAppliedPaddings@.
calculateAppliedPaddings
  :: Rect -> Double -> Double -> Double -> Double -> Double
  -> ParsedPaddings
calculateAppliedPaddings bounds x y zoom width height =
  let vp = Viewport x y zoom
      XYPosition left top =
        rendererPointToPoint (XYPosition (rectX bounds) (rectY bounds)) vp
      XYPosition boundRight boundBottom =
        rendererPointToPoint
          (XYPosition (rectX bounds + rectWidth bounds)
                      (rectY bounds + rectHeight bounds)) vp
      right = width - boundRight
      bottom = height - boundBottom
      f v = fromIntegral (floor v :: Integer)
  in ParsedPaddings (f top) (f right) (f bottom) (f left)
       (f left + f right) (f top + f bottom)
-----------------------------------------------------------------------------
-- | Viewport that encloses the given bounds with padding; port of
-- @getViewportForBounds@.
getViewportForBounds
  :: Rect
  -- ^ bounds to fit
  -> Double
  -- ^ viewport width
  -> Double
  -- ^ viewport height
  -> Double
  -- ^ min zoom
  -> Double
  -- ^ max zoom
  -> Padding
  -> Viewport
getViewportForBounds bounds width height minZoom maxZoom padding =
  let p = parsePaddings padding width height
      xZoom = (width - ppX p) / rectWidth bounds
      yZoom = (height - ppY p) / rectHeight bounds
      zoom = min xZoom yZoom
      clampedZoom = clamp zoom minZoom maxZoom
      boundsCenterX = rectX bounds + rectWidth bounds / 2
      boundsCenterY = rectY bounds + rectHeight bounds / 2
      x = width / 2 - boundsCenterX * clampedZoom
      y = height / 2 - boundsCenterY * clampedZoom
      newPadding = calculateAppliedPaddings bounds x y clampedZoom width height
      offLeft = min (ppLeft newPadding - ppLeft p) 0
      offTop = min (ppTop newPadding - ppTop p) 0
      offRight = min (ppRight newPadding - ppRight p) 0
      offBottom = min (ppBottom newPadding - ppBottom p) 0
  in Viewport
      { viewportX = x - offLeft + offRight
      , viewportY = y - offTop + offBottom
      , viewportZoom = clampedZoom
      }
-----------------------------------------------------------------------------
-- | @measured.width ?? width ?? initialWidth ?? 0@ (and same for height).
getNodeDimensions :: Node n -> Dimensions
getNodeDimensions n = Dimensions
  (fromMaybe 0 (firstJust [measuredWidth =<< nodeMeasured n, nodeWidth n, nodeInitialWidth n]))
  (fromMaybe 0 (firstJust [measuredHeight =<< nodeMeasured n, nodeHeight n, nodeInitialHeight n]))
-----------------------------------------------------------------------------
-- | 'getNodeDimensions' with the internal @measured@ taking precedence.
getInternalNodeDimensions :: InternalNode n -> Dimensions
getInternalNodeDimensions n = Dimensions
  (fromMaybe 0 (firstJust
    [ measuredWidth (internalMeasured n)
    , nodeWidth u, nodeInitialWidth u ]))
  (fromMaybe 0 (firstJust
    [ measuredHeight (internalMeasured n)
    , nodeHeight u, nodeInitialHeight u ]))
  where u = internalUser n
-----------------------------------------------------------------------------
nodeHasDimensions :: Node n -> Bool
nodeHasDimensions n =
  hasJust [measuredWidth =<< nodeMeasured n, nodeWidth n, nodeInitialWidth n]
  && hasJust [measuredHeight =<< nodeMeasured n, nodeHeight n, nodeInitialHeight n]
  where hasJust = any (/= Nothing)
-----------------------------------------------------------------------------
-- | Convert a child position to an absolute position; port of
-- @evaluateAbsolutePosition@.
evaluateAbsolutePosition
  :: XYPosition
  -> Maybe Dimensions
  -- ^ dimensions (defaults to 0x0)
  -> NodeId
  -- ^ parent id
  -> NodeLookup n
  -> NodeOrigin
  -> XYPosition
evaluateAbsolutePosition position dims parentId nodeLookup nodeOrigin' =
  case M.lookup parentId nodeLookup of
    Nothing -> position
    Just parent ->
      let NodeOrigin ox oy =
            fromMaybe nodeOrigin' (nodeOrigin (internalUser parent))
          Dimensions w h = fromMaybe zeroDimensions dims
          XYPosition px py = internalPositionAbsolute parent
      in XYPosition
           (xyX position + px - w * ox)
           (xyY position + py - h * oy)
-----------------------------------------------------------------------------
-- | Position of a node adjusted by its origin; local copy to avoid a
-- module cycle with "Miso.Flow.Utils.Graph" (which re-exports the public
-- version).
getNodePositionWithOriginLocal :: Node n -> NodeOrigin -> XYPosition
getNodePositionWithOriginLocal n nodeOrigin' =
  let Dimensions w h = getNodeDimensions n
      NodeOrigin ox oy = fromMaybe nodeOrigin' (nodeOrigin n)
  in XYPosition (xyX (nodePosition n) - w * ox) (xyY (nodePosition n) - h * oy)
-----------------------------------------------------------------------------
firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (\m acc -> maybe acc Just m) Nothing
-----------------------------------------------------------------------------
