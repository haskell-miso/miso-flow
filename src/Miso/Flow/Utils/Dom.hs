-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Dom
-- License     :  BSD3-style (see the file LICENSE)
--
-- Port of @utils\/dom.ts@ from @\@xyflow\/system@. These functions need a
-- live DOM, so unlike the rest of "Miso.Flow.Utils" they run in 'IO'
-- (call them from @update@ effects, not from @view@).
----------------------------------------------------------------------------
module Miso.Flow.Utils.Dom
  ( getDimensions
  , getBoundingRect
  , getHostForElement
  , isInputDOMNode
  , isMouseEvent
  , getEventPosition
  , PointerPosition (..)
  , getPointerPosition
  , getHandleBounds
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (forM, filterM)
import           Data.Maybe (fromMaybe)
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Effect (DOMRef)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Miso.Flow.Types
import           Miso.Flow.Utils.General
  ( pointToRendererPoint
  , snapPosition
  )
-----------------------------------------------------------------------------
-- | @offsetWidth@ \/ @offsetHeight@ of an element.
getDimensions :: DOMRef -> IO Dimensions
getDimensions el = do
  w <- fromJSValUnchecked =<< el ! "offsetWidth"
  h <- fromJSValUnchecked =<< el ! "offsetHeight"
  pure (Dimensions w h)
-----------------------------------------------------------------------------
-- | @getBoundingClientRect()@ as a 'Rect'.
getBoundingRect :: DOMRef -> IO Rect
getBoundingRect el = do
  r <- el # "getBoundingClientRect" $ ()
  x <- fromJSValUnchecked =<< r ! "left"
  y <- fromJSValUnchecked =<< r ! "top"
  w <- fromJSValUnchecked =<< r ! "width"
  h <- fromJSValUnchecked =<< r ! "height"
  pure (Rect x y w h)
-----------------------------------------------------------------------------
-- | Root node (document or shadow root) hosting the element.
getHostForElement :: DOMRef -> IO JSVal
getHostForElement el = do
  root <- el # "getRootNode" $ ()
  undef <- isUndefined root
  if undef then jsg "document" else pure root
-----------------------------------------------------------------------------
-- | 'True' when the (keyboard) event targets an input-like element or an
-- element inside @.nokey@.
isInputDOMNode :: JSVal -> IO Bool
isInputDOMNode event = do
  path <- event # "composedPath" $ ()
  pathHead <- path !! 0
  headUndef <- isUndefined pathHead
  target <- if headUndef then event ! "target" else pure pathHead
  targetNull <- isNull target
  targetUndef <- isUndefined target
  if targetNull || targetUndef
    then pure False
    else do
      nodeType <- fromJSVal =<< target ! "nodeType"
      if nodeType /= Just (1 :: Int)
        then pure False
        else do
          name <- fromJSValUnchecked =<< target ! "nodeName"
          editable <- fromJSValUnchecked =<<
            (target # "hasAttribute" $ ["contenteditable" :: MisoString])
          closest <- target # "closest" $ [".nokey" :: MisoString]
          inNoKey <- not <$> isNull closest
          pure $ name `elem` (["INPUT", "SELECT", "TEXTAREA"] :: [MisoString])
                   || editable
                   || inNoKey
-----------------------------------------------------------------------------
-- | 'True' when the event carries mouse (not touch) coordinates.
isMouseEvent :: JSVal -> IO Bool
isMouseEvent event = do
  x <- event ! "clientX"
  not <$> isUndefined x
-----------------------------------------------------------------------------
-- | Client position of a mouse or touch event, optionally relative to
-- the given bounds.
getEventPosition :: JSVal -> Maybe Rect -> IO XYPosition
getEventPosition event mBounds = do
  mouse <- isMouseEvent event
  (x, y) <-
    if mouse
      then do
        x <- fromJSValUnchecked =<< event ! "clientX"
        y <- fromJSValUnchecked =<< event ! "clientY"
        pure (x, y)
      else do
        touches <- event ! "touches"
        t0 <- touches !! 0
        x <- fromJSValUnchecked =<< t0 ! "clientX"
        y <- fromJSValUnchecked =<< t0 ! "clientY"
        pure (x, y)
  let bx = maybe 0 rectX mBounds
      by = maybe 0 rectY mBounds
  pure (XYPosition (x - bx) (y - by))
-----------------------------------------------------------------------------
-- | Pointer position in flow coordinates, with the snapped variant used
-- to skip no-movement drag events.
data PointerPosition = PointerPosition
  { pointerPosition :: !XYPosition
  , pointerXSnapped :: !Double
  , pointerYSnapped :: !Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Port of @getPointerPosition@.
getPointerPosition
  :: JSVal
  -- ^ mouse or touch event
  -> Transform
  -> Bool
  -- ^ snap to grid
  -> Maybe SnapGrid
  -> Maybe Rect
  -- ^ container bounds
  -> IO PointerPosition
getPointerPosition event transform snapToGrid mSnapGrid mBounds = do
  XYPosition x y <- getEventPosition event Nothing
  let pointerPos = pointToRendererPoint
        (XYPosition (x - maybe 0 rectX mBounds) (y - maybe 0 rectY mBounds))
        transform
        False
        (SnapGrid 1 1)
      snapped =
        if snapToGrid
          then snapPosition pointerPos (fromMaybe (SnapGrid 0 0) mSnapGrid)
          else pointerPos
  pure PointerPosition
    { pointerPosition = pointerPos
    , pointerXSnapped = xyX snapped
    , pointerYSnapped = xyY snapped
    }
-----------------------------------------------------------------------------
-- | Measure the handles of the given type inside a node element,
-- relative to the node; port of @getHandleBounds@.
getHandleBounds
  :: HandleType
  -> DOMRef
  -- ^ node element
  -> Rect
  -- ^ node bounds (client rect)
  -> Double
  -- ^ zoom
  -> NodeId
  -> IO (Maybe [Handle])
getHandleBounds handleType nodeElement nodeBounds zoom nid = do
  handleList <- nodeElement # "querySelectorAll"
    $ [ "." <> handleTypeToText handleType ]
  len <- fromJSValUnchecked =<< handleList ! "length"
  if (len :: Int) == 0
    then pure Nothing
    else do
      els <- filterM (fmap not . isUndefined) =<<
        forM [0 .. len - 1] (handleList !!)
      handles <- forM els $ \handle -> do
        bounds <- handle # "getBoundingClientRect" $ ()
        left <- fromJSValUnchecked =<< bounds ! "left"
        top <- fromJSValUnchecked =<< bounds ! "top"
        hid <- fromJSVal =<< (handle # "getAttribute" $ ["data-handleid" :: MisoString])
        posAttr <- fromJSVal =<< (handle # "getAttribute" $ ["data-handlepos" :: MisoString])
        Dimensions w h <- getDimensions handle
        pure Handle
          { hId = hid
          , hNodeId = nid
          , hPosition = fromMaybe PositionBottom (positionFromText =<< posAttr)
          , hX = (left - rectX nodeBounds) / zoom
          , hY = (top - rectY nodeBounds) / zoom
          , hType = handleType
          , hWidth = w
          , hHeight = h
          }
      pure (Just handles)
-----------------------------------------------------------------------------
