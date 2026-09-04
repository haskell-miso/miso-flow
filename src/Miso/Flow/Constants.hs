-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Constants
-- License     :  BSD3-style (see the file LICENSE)
--
-- Port of @constants.ts@ from @\@xyflow\/system@.
----------------------------------------------------------------------------
module Miso.Flow.Constants
  ( -- * Error messages
    errorMessage
  , error001, error002, error003, error004, error005, error006, error007
  , error008, error009, error010, error011, error012, error013, error014
  , error015, error016
    -- * Extents
  , infiniteExtent
    -- * Keys
  , elementSelectionKeys
    -- * Aria labels
  , AriaLabelConfig (..)
  , defaultAriaLabelConfig
  , mergeAriaLabelConfig
  ) where
-----------------------------------------------------------------------------
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | Look up an error message renderer by its code. The context argument
-- (an id, node type, etc.) is interpolated where the original message
-- takes a parameter and ignored otherwise.
errorMessage :: ErrorCode -> MisoString -> MisoString
errorMessage code arg =
  case code of
    "001" -> error001
    "002" -> error002
    "003" -> error003 arg
    "004" -> error004
    "005" -> error005
    "006" -> error006
    "007" -> error007 arg
    "009" -> error009 arg
    "010" -> error010
    "011" -> error011 arg
    "012" -> error012 arg
    "013" -> error013
    "014" -> error014
    "015" -> error015
    "016" -> error016 arg
    _     -> "Unknown error code: " <> code
-----------------------------------------------------------------------------
error001 :: MisoString
error001 = "Seems like you have not used a FlowProvider as an ancestor."
-----------------------------------------------------------------------------
error002 :: MisoString
error002 = mconcat
  [ "It looks like you've created a new nodeTypes or edgeTypes object. "
  , "If this wasn't on purpose please define the nodeTypes/edgeTypes "
  , "outside of the component or memoize them."
  ]
-----------------------------------------------------------------------------
error003 :: MisoString -> MisoString
error003 nodeType = mconcat
  [ "Node type \"", nodeType, "\" not found. Using fallback type \"default\"." ]
-----------------------------------------------------------------------------
error004 :: MisoString
error004 = "The parent container needs a width and a height to render the graph."
-----------------------------------------------------------------------------
error005 :: MisoString
error005 = "Only child nodes can use a parent extent."
-----------------------------------------------------------------------------
error006 :: MisoString
error006 = "Can't create edge. An edge needs a source and a target."
-----------------------------------------------------------------------------
error007 :: MisoString -> MisoString
error007 i = mconcat [ "The old edge with id=", i, " does not exist." ]
-----------------------------------------------------------------------------
-- | Takes the failing 'HandleType', the handle id (if any) and the edge id.
error008 :: HandleType -> Maybe MisoString -> EdgeId -> MisoString
error008 handleType handleId edgeId' = mconcat
  [ "Couldn't create edge for ", handleTypeToText handleType
  , " handle id: \"", maybe "null" id handleId
  , "\", edge id: ", edgeId', "."
  ]
-----------------------------------------------------------------------------
error009 :: MisoString -> MisoString
error009 t = mconcat [ "Marker type \"", t, "\" doesn't exist." ]
-----------------------------------------------------------------------------
error010 :: MisoString
error010 =
  "Handle: No node id found. Make sure to only use a Handle inside a custom Node."
-----------------------------------------------------------------------------
error011 :: MisoString -> MisoString
error011 edgeType = mconcat
  [ "Edge type \"", edgeType, "\" not found. Using fallback type \"default\"." ]
-----------------------------------------------------------------------------
error012 :: MisoString -> MisoString
error012 i = mconcat
  [ "Node with id \"", i, "\" does not exist, it may have been removed. "
  , "This can happen when a node is deleted before the \"onNodeClick\" "
  , "handler is called."
  ]
-----------------------------------------------------------------------------
error013 :: MisoString
error013 = "It seems that you haven't loaded the miso-flow styles."
-----------------------------------------------------------------------------
error014 :: MisoString
error014 = mconcat
  [ "useNodeConnections: No node ID found. Call useNodeConnections inside "
  , "a custom Node or provide a node ID." ]
-----------------------------------------------------------------------------
error015 :: MisoString
error015 = mconcat
  [ "It seems that you are trying to drag a node that is not initialized. "
  , "Please use onNodesChange as explained in the docs." ]
-----------------------------------------------------------------------------
error016 :: MisoString -> MisoString
error016 i = mconcat
  [ "Edge with id \"", i, "\" does not exist, it may have been removed. "
  , "This can happen when an edge is deleted before the \"onEdgeClick\" "
  , "handler is called."
  ]
-----------------------------------------------------------------------------
-- | The unbounded 'CoordinateExtent'.
infiniteExtent :: CoordinateExtent
infiniteExtent = CoordinateExtent
  (-1 / 0) (-1 / 0) (1 / 0) (1 / 0)
-----------------------------------------------------------------------------
elementSelectionKeys :: [MisoString]
elementSelectionKeys = [ "Enter", " ", "Escape" ]
-----------------------------------------------------------------------------
-- | Port of @defaultAriaLabelConfig@. The two message functions are
-- proper Haskell functions.
data AriaLabelConfig = AriaLabelConfig
  { ariaNodeDescriptionDefault          :: MisoString
  , ariaNodeDescriptionKeyboardDisabled :: MisoString
  , ariaNodeLiveMessage                 :: MisoString -> Double -> Double -> MisoString
    -- ^ direction, x, y
  , ariaEdgeDescriptionDefault          :: MisoString
  , ariaControlsLabel                   :: MisoString
  , ariaControlsZoomInLabel             :: MisoString
  , ariaControlsZoomOutLabel            :: MisoString
  , ariaControlsFitViewLabel            :: MisoString
  , ariaControlsInteractiveLabel        :: MisoString
  , ariaMinimapLabel                    :: MisoString
  , ariaHandleLabel                     :: MisoString
  }
-----------------------------------------------------------------------------
defaultAriaLabelConfig :: AriaLabelConfig
defaultAriaLabelConfig = AriaLabelConfig
  { ariaNodeDescriptionDefault = mconcat
      [ "Press enter or space to select a node. "
      , "Press delete to remove it and escape to cancel." ]
  , ariaNodeDescriptionKeyboardDisabled = mconcat
      [ "Press enter or space to select a node. "
      , "You can then use the arrow keys to move the node around. "
      , "Press delete to remove it and escape to cancel." ]
  , ariaNodeLiveMessage = \direction x y -> mconcat
      [ "Moved selected node ", direction
      , ". New position, x: ", ms (round x :: Int)
      , ", y: ", ms (round y :: Int) ]
  , ariaEdgeDescriptionDefault = mconcat
      [ "Press enter or space to select an edge. "
      , "You can then press delete to remove it or escape to cancel." ]
  , ariaControlsLabel = "Control Panel"
  , ariaControlsZoomInLabel = "Zoom In"
  , ariaControlsZoomOutLabel = "Zoom Out"
  , ariaControlsFitViewLabel = "Fit View"
  , ariaControlsInteractiveLabel = "Toggle Interactivity"
  , ariaMinimapLabel = "Mini Map"
  , ariaHandleLabel = "Handle"
  }
-----------------------------------------------------------------------------
-- | Port of @mergeAriaLabelConfig@: apply overrides to the defaults.
mergeAriaLabelConfig
  :: (AriaLabelConfig -> AriaLabelConfig)
  -> AriaLabelConfig
mergeAriaLabelConfig override = override defaultAriaLabelConfig
-----------------------------------------------------------------------------
