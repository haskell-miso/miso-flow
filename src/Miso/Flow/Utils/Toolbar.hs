-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Toolbar
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/node-toolbar.ts@ and @utils\/edge-toolbar.ts@
-- from @\@xyflow\/system@: CSS transforms for toolbars rendered next to
-- nodes and edges.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Toolbar
  ( AlignX (..)
  , AlignY (..)
  , getEdgeToolbarTransform
  , getNodeToolbarTransform
  ) where
-----------------------------------------------------------------------------
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Miso.Flow.Internal.JSNum (jsShow)
import           Miso.Flow.Types
-----------------------------------------------------------------------------
data AlignX = AlignXLeft | AlignXCenter | AlignXRight
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data AlignY = AlignYTop | AlignYCenter | AlignYBottom
  deriving (Show, Eq)
-----------------------------------------------------------------------------
alignXToPercent :: AlignX -> Double
alignXToPercent = \case
  AlignXLeft -> 0
  AlignXCenter -> 50
  AlignXRight -> 100
-----------------------------------------------------------------------------
alignYToPercent :: AlignY -> Double
alignYToPercent = \case
  AlignYTop -> 0
  AlignYCenter -> 50
  AlignYBottom -> 100
-----------------------------------------------------------------------------
-- | CSS transform for an edge toolbar at flow position @(x, y)@; port of
-- @getEdgeToolbarTransform@.
getEdgeToolbarTransform
  :: Double -- ^ x
  -> Double -- ^ y
  -> Double -- ^ zoom
  -> AlignX -- ^ default 'AlignXCenter'
  -> AlignY -- ^ default 'AlignYCenter'
  -> MisoString
getEdgeToolbarTransform x y zoom alignX alignY = mconcat
  [ "translate(", jsShow x, "px, ", jsShow y, "px) scale("
  , jsShow (1 / zoom), ") translate("
  , jsShow (negate (alignXToPercent alignX)), "%, "
  , jsShow (negate (alignYToPercent alignY)), "%)"
  ]
-----------------------------------------------------------------------------
-- | CSS transform for a node toolbar; port of @getNodeToolbarTransform@.
getNodeToolbarTransform
  :: Rect      -- ^ node rect (flow coordinates)
  -> Viewport
  -> Position  -- ^ side of the node
  -> Double    -- ^ offset
  -> Align
  -> MisoString
getNodeToolbarTransform nodeRect vp position offset align = mconcat
  [ "translate(", jsShow posX, "px, ", jsShow posY, "px) translate("
  , jsShow shiftX, "%, ", jsShow shiftY, "%)"
  ]
  where
    alignmentOffset :: Double
    alignmentOffset = case align of
      AlignStart -> 0
      AlignEnd -> 1
      AlignCenter -> 0.5
    zoom = viewportZoom vp
    -- defaults are the Position.Top case
    topX = (rectX nodeRect + rectWidth nodeRect * alignmentOffset) * zoom + viewportX vp
    topY = rectY nodeRect * zoom + viewportY vp - offset
    (posX, posY, shiftX, shiftY) =
      case position of
        PositionTop ->
          (topX, topY, -100 * alignmentOffset, -100)
        PositionRight ->
          ( (rectX nodeRect + rectWidth nodeRect) * zoom + viewportX vp + offset
          , (rectY nodeRect + rectHeight nodeRect * alignmentOffset) * zoom + viewportY vp
          , 0
          , -100 * alignmentOffset )
        PositionBottom ->
          ( topX
          , (rectY nodeRect + rectHeight nodeRect) * zoom + viewportY vp + offset
          , -100 * alignmentOffset
          , 0 )
        PositionLeft ->
          ( rectX nodeRect * zoom + viewportX vp - offset
          , (rectY nodeRect + rectHeight nodeRect * alignmentOffset) * zoom + viewportY vp
          , -100
          , -100 * alignmentOffset )
-----------------------------------------------------------------------------
