-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
#ifdef WASM
{-# LANGUAGE TemplateHaskell   #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Internal.Bridge
-- License     :  BSD3-style (see the file LICENSE)
--
-- Low-level bindings to the miso-flow JavaScript bridge
-- (@js\/miso-flow.js@, built from @ts\/miso-flow.ts@), which drives the
-- imperative @\@xyflow\/system@ modules (XYPanZoom, XYDrag, XYHandle,
-- XYResizer, XYMinimap) against the DOM rendered by miso.
--
-- Everything crossing the boundary is JSON. Higher layers
-- ("Miso.Flow.Component" and the @Miso.Flow.*@ instance modules) wrap
-- this into MVU-friendly interfaces.
----------------------------------------------------------------------------
module Miso.Flow.Internal.Bridge
  ( -- * Store handle
    FlowStore (..)
    -- * Options
  , StoreOptions (..)
  , defaultStoreOptions
    -- * Callbacks out of the gesture system
  , BridgeCallbacks (..)
  , emptyBridgeCallbacks
    -- * Wire types (JS -> Haskell payloads)
  , WireNodeChange (..)
  , ConnectionStateWire (..)
  , wireConnectionState
    -- * Store lifecycle
  , createFlowStore
  , storeDestroy
    -- * Graph sync
  , storeSetNodes
  , storeSetEdges
  , storeUpdateOptions
    -- * Viewport control
  , storeGetViewport
  , storeSetViewport
  , storeSyncViewport
  , storeZoomIn
  , storeZoomOut
  , storeZoomTo
  , storeScaleBy
  , storeSetCenter
  , storeFitView
  , storeFitBounds
  , storePanBy
    -- * Node measurement
  , storeObserveNode
  , storeUnobserveNode
  , storeRequestNodeMeasure
    -- * Dragging
  , storeAttachNodeDrag
  , storeUpdateNodeDrag
  , storeDetachNodeDrag
    -- * Connections
  , storeAttachHandle
  , storeAttachReconnectAnchor
    -- * Resizer
  , storeAttachResizer
  , storeUpdateResizer
  , storeDetachResizer
    -- * Minimap
  , storeAttachMinimap
  , storeUpdateMinimap
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Maybe (fromMaybe)
import           Prelude
#ifdef WASM
import           Control.Monad (unless)
import           Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import           System.IO.Unsafe (unsafePerformIO)
#endif
-----------------------------------------------------------------------------
import           Miso.DSL
#ifdef WASM
import           Miso.DSL.TH.File (evalFile)
#endif
import           Miso.Effect (DOMRef)
import           Miso.JSON
  ( FromJSON (..)
  , ToJSON (..)
  , Value (Bool, String)
  , decode
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Miso.Flow.Internal.JSNum (jsShow)
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | Handle to a JavaScript-side @MisoFlowStore@.
newtype FlowStore = FlowStore JSVal
-----------------------------------------------------------------------------
-- | A component holds at most one store for its whole lifetime, so all
-- handles are interchangeable; this lets models containing a 'FlowStore'
-- satisfy the @Eq model@ constraint of the miso runtime.
instance Eq FlowStore where
  _ == _ = True
-----------------------------------------------------------------------------
-- | Options handed to the JavaScript store; mirrors the union of pane,
-- drag and connection settings across @\@xyflow\/system@'s modules.
data StoreOptions = StoreOptions
  { soFlowId                  :: MisoString
  , soLib                     :: MisoString
  , soMinZoom                 :: Double
  , soMaxZoom                 :: Double
  , soTranslateExtent         :: Maybe CoordinateExtent
  , soNodeExtent              :: Maybe CoordinateExtent
  , soNodeOrigin              :: NodeOrigin
  , soDefaultViewport         :: Viewport
  , soSnapToGrid              :: Bool
  , soSnapGrid                :: SnapGrid
  , soElevateNodesOnSelect    :: Bool
  , soZIndexMode              :: ZIndexMode
  , soNodesDraggable          :: Bool
  , soAutoPanOnNodeDrag       :: Bool
  , soAutoPanOnConnect        :: Bool
  , soAutoPanSpeed            :: Double
  , soNodeDragThreshold       :: Double
  , soNodeClickDistance       :: Double
  , soSelectNodesOnDrag       :: Bool
  , soConnectionMode          :: ConnectionMode
  , soConnectionRadius        :: Double
  , soConnectionDragThreshold :: Double
  , soPanOnDrag               :: PanOnDrag
  , soPanOnScroll             :: Bool
  , soPanOnScrollMode         :: PanOnScrollMode
  , soPanOnScrollSpeed        :: Double
  , soZoomOnScroll            :: Bool
  , soZoomOnPinch             :: Bool
  , soZoomOnDoubleClick       :: Bool
  , soPreventScrolling        :: Bool
  , soPaneClickDistance       :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Defaults matching xyflow's.
defaultStoreOptions :: StoreOptions
defaultStoreOptions = StoreOptions
  { soFlowId = "1"
  , soLib = "miso"
  , soMinZoom = 0.5
  , soMaxZoom = 2
  , soTranslateExtent = Nothing
  , soNodeExtent = Nothing
  , soNodeOrigin = defaultOrigin
  , soDefaultViewport = defaultViewport
  , soSnapToGrid = False
  , soSnapGrid = SnapGrid 15 15
  , soElevateNodesOnSelect = True
  , soZIndexMode = ZIndexBasic
  , soNodesDraggable = True
  , soAutoPanOnNodeDrag = True
  , soAutoPanOnConnect = True
  , soAutoPanSpeed = 15
  , soNodeDragThreshold = 1
  , soNodeClickDistance = 0
  , soSelectNodesOnDrag = True
  , soConnectionMode = ConnectionModeStrict
  , soConnectionRadius = 20
  , soConnectionDragThreshold = 1
  , soPanOnDrag = PanOnDrag True
  , soPanOnScroll = False
  , soPanOnScrollMode = PanOnScrollFree
  , soPanOnScrollSpeed = 0.5
  , soZoomOnScroll = True
  , soZoomOnPinch = True
  , soZoomOnDoubleClick = True
  , soPreventScrolling = True
  , soPaneClickDistance = 0
  }
-----------------------------------------------------------------------------
instance ToJSON StoreOptions where
  toJSON StoreOptions {..} = object
    [ "flowId" .= soFlowId
    , "lib" .= soLib
    , "minZoom" .= soMinZoom
    , "maxZoom" .= soMaxZoom
    , "translateExtent" .= soTranslateExtent
    , "nodeExtent" .= soNodeExtent
    , "nodeOrigin" .= soNodeOrigin
    , "defaultViewport" .= soDefaultViewport
    , "snapToGrid" .= soSnapToGrid
    , "snapGrid" .= soSnapGrid
    , "elevateNodesOnSelect" .= soElevateNodesOnSelect
    , "zIndexMode" .= soZIndexMode
    , "nodesDraggable" .= soNodesDraggable
    , "autoPanOnNodeDrag" .= soAutoPanOnNodeDrag
    , "autoPanOnConnect" .= soAutoPanOnConnect
    , "autoPanSpeed" .= soAutoPanSpeed
    , "nodeDragThreshold" .= soNodeDragThreshold
    , "nodeClickDistance" .= soNodeClickDistance
    , "selectNodesOnDrag" .= soSelectNodesOnDrag
    , "connectionMode" .= soConnectionMode
    , "connectionRadius" .= soConnectionRadius
    , "connectionDragThreshold" .= soConnectionDragThreshold
    , "panOnDrag" .= soPanOnDrag
    , "panOnScroll" .= soPanOnScroll
    , "panOnScrollMode" .= soPanOnScrollMode
    , "panOnScrollSpeed" .= soPanOnScrollSpeed
    , "zoomOnScroll" .= soZoomOnScroll
    , "zoomOnPinch" .= soZoomOnPinch
    , "zoomOnDoubleClick" .= soZoomOnDoubleClick
    , "preventScrolling" .= soPreventScrolling
    , "paneClickDistance" .= soPaneClickDistance
    ]
-----------------------------------------------------------------------------
-- | One node change as reported by the JavaScript side (drag steps,
-- measurements, resizes). Unlike 'NodeChange' this can carry the
-- DOM-measured handle bounds and absolute position.
data WireNodeChange
  = WirePositionChange NodeId (Maybe XYPosition) (Maybe XYPosition) (Maybe Bool)
    -- ^ id, position, positionAbsolute, dragging
  | WireDimensionChange NodeId (Maybe Dimensions) (Maybe Bool) SetAttributes
      (Maybe NodeHandleBounds) (Maybe XYPosition)
    -- ^ id, dimensions, resizing, setAttributes, handleBounds, positionAbsolute
  | WireSelectionChange NodeId Bool
  | WireRemoveChange NodeId
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON WireNodeChange where
  parseJSON = withObject "WireNodeChange" $ \o -> do
    ty <- o .: "type"
    case ty :: MisoString of
      "position" ->
        WirePositionChange
          <$> o .: "id"
          <*> o .:? "position"
          <*> o .:? "positionAbsolute"
          <*> o .:? "dragging"
      "dimensions" ->
        WireDimensionChange
          <$> o .: "id"
          <*> o .:? "dimensions"
          <*> o .:? "resizing"
          <*> (parseSetAttributes <$> o .:? "setAttributes")
          <*> o .:? "handleBounds"
          <*> o .:? "positionAbsolute"
      "select" ->
        WireSelectionChange <$> o .: "id" <*> o .: "selected"
      "remove" ->
        WireRemoveChange <$> o .: "id"
      _ -> fail "unknown node change type"
    where
      parseSetAttributes = \case
        Nothing -> SetAttributesNone
        Just (Bool True) -> SetAttributesBoth
        Just (Bool False) -> SetAttributesNone
        Just (String "width") -> SetAttributesWidth
        Just (String "height") -> SetAttributesHeight
        Just _ -> SetAttributesNone
-----------------------------------------------------------------------------
-- | Connection state as serialized by the bridge: nodes are referred to
-- by id (resolve them against your 'NodeLookup' via
-- 'wireConnectionState'). Mirroring @\@xyflow\/system@, 'cwFrom' is in
-- flow coordinates while 'cwTo' and 'cwPointer' arrive in pane (screen)
-- coordinates — convert them with
-- 'Miso.Flow.Utils.General.pointToRendererPoint' before rendering.
data ConnectionStateWire = ConnectionStateWire
  { cwInProgress   :: Bool
  , cwIsValid      :: Maybe Bool
  , cwFrom         :: Maybe XYPosition
  , cwFromHandle   :: Maybe Handle
  , cwFromPosition :: Maybe Position
  , cwFromNode     :: Maybe NodeId
  , cwTo           :: Maybe XYPosition
  , cwToHandle     :: Maybe Handle
  , cwToPosition   :: Maybe Position
  , cwToNode       :: Maybe NodeId
  , cwPointer      :: Maybe XYPosition
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON ConnectionStateWire where
  parseJSON = withObject "ConnectionStateWire" $ \o ->
    ConnectionStateWire
      <$> (o .: "inProgress" <|> pure False)
      <*> o .:? "isValid"
      <*> o .:? "from"
      <*> o .:? "fromHandle"
      <*> o .:? "fromPosition"
      <*> o .:? "fromNode"
      <*> o .:? "to"
      <*> o .:? "toHandle"
      <*> o .:? "toPosition"
      <*> o .:? "toNode"
      <*> o .:? "pointer"
-----------------------------------------------------------------------------
-- | Resolve a 'ConnectionStateWire' into a 'ConnectionState' using a
-- node resolver (typically a 'Data.Map.Strict.lookup' into your
-- 'NodeLookup').
wireConnectionState
  :: (NodeId -> Maybe (InternalNode n))
  -> ConnectionStateWire
  -> ConnectionState n
wireConnectionState resolve ConnectionStateWire {..}
  | cwInProgress
  , Just from <- cwFrom
  , Just fromHandle <- cwFromHandle
  , Just fromPosition <- cwFromPosition
  , Just fromNode <- resolve =<< cwFromNode
  , Just to <- cwTo
  , Just toPosition <- cwToPosition
  , Just pointer <- cwPointer
  = InProgress ConnectionInProgress
      { cipIsValid = cwIsValid
      , cipFrom = from
      , cipFromHandle = fromHandle
      , cipFromPosition = fromPosition
      , cipFromNode = fromNode
      , cipTo = to
      , cipToHandle = cwToHandle
      , cipToPosition = toPosition
      , cipToNode = resolve =<< cwToNode
      , cipPointer = pointer
      }
  | otherwise = NoConnection
-----------------------------------------------------------------------------
-- | Haskell handlers for events raised by the gesture system. Payloads
-- arrive parsed; hook the ones you need and leave the rest as no-ops
-- (see 'emptyBridgeCallbacks').
data BridgeCallbacks = BridgeCallbacks
  { bcViewport         :: Viewport -> IO ()
  , bcViewportStart    :: Viewport -> IO ()
  , bcViewportEnd      :: Viewport -> IO ()
  , bcNodeChanges      :: [WireNodeChange] -> IO ()
  , bcConnectionUpdate :: ConnectionStateWire -> IO ()
  , bcConnectStart     :: OnConnectStartParams -> IO ()
  , bcConnect          :: Connection -> IO ()
  , bcConnectEnd       :: ConnectionStateWire -> IO ()
  , bcNodeMouseDown    :: NodeId -> Bool -> IO ()
    -- ^ id and whether a multi-selection modifier was held
  , bcUnselectAll      :: IO ()
  , bcPaneClick        :: IO ()
  , bcNodeDragStart    :: NodeId -> IO ()
  , bcNodeDragStop     :: NodeId -> IO ()
  , bcResizeChanges    :: [WireNodeChange] -> IO ()
  , bcDimensions       :: Dimensions -> IO ()
    -- ^ container size, on creation and on resize
  , bcSelectionRect    :: Rect -> IO ()
    -- ^ selection box changed (container coordinates)
  , bcSelectionEnd     :: Rect -> IO ()
    -- ^ selection box released
  , bcReconnect        :: EdgeId -> Connection -> IO ()
    -- ^ an edge anchor was dragged onto a new valid handle
  , bcResizeStart      :: NodeId -> IO ()
  , bcResizeEnd        :: NodeId -> IO ()
  , bcMinimapClick     :: XYPosition -> IO ()
    -- ^ click on the minimap, in flow coordinates
  , bcDeleteKey        :: IO ()
    -- ^ Delete\/Backspace pressed outside an input
  , bcError            :: MisoString -> MisoString -> IO ()
  }
-----------------------------------------------------------------------------
emptyBridgeCallbacks :: BridgeCallbacks
emptyBridgeCallbacks = BridgeCallbacks
  { bcViewport = \_ -> pure ()
  , bcViewportStart = \_ -> pure ()
  , bcViewportEnd = \_ -> pure ()
  , bcNodeChanges = \_ -> pure ()
  , bcConnectionUpdate = \_ -> pure ()
  , bcConnectStart = \_ -> pure ()
  , bcConnect = \_ -> pure ()
  , bcConnectEnd = \_ -> pure ()
  , bcNodeMouseDown = \_ _ -> pure ()
  , bcUnselectAll = pure ()
  , bcPaneClick = pure ()
  , bcNodeDragStart = \_ -> pure ()
  , bcNodeDragStop = \_ -> pure ()
  , bcResizeChanges = \_ -> pure ()
  , bcDimensions = \_ -> pure ()
  , bcSelectionRect = \_ -> pure ()
  , bcSelectionEnd = \_ -> pure ()
  , bcReconnect = \_ _ -> pure ()
  , bcResizeStart = \_ -> pure ()
  , bcResizeEnd = \_ -> pure ()
  , bcMinimapClick = \_ -> pure ()
  , bcDeleteKey = pure ()
  , bcError = \_ _ -> pure ()
  }
-----------------------------------------------------------------------------
-- | Wrap a JSON-payload handler as a JS callback.
jsonCallback :: FromJSON a => (a -> IO ()) -> IO JSVal
jsonCallback handler =
  asyncCallback1 $ \payload -> do
    mStr <- fromJSVal payload
    case decode =<< mStr of
      Just parsed -> handler parsed
      Nothing -> pure ()
-----------------------------------------------------------------------------
newtype IdPayload = IdPayload NodeId
-----------------------------------------------------------------------------
instance FromJSON IdPayload where
  parseJSON = withObject "IdPayload" $ \o -> IdPayload <$> o .: "id"
-----------------------------------------------------------------------------
data MouseDownPayload = MouseDownPayload NodeId Bool
-----------------------------------------------------------------------------
instance FromJSON MouseDownPayload where
  parseJSON = withObject "MouseDownPayload" $ \o ->
    MouseDownPayload
      <$> o .: "id"
      <*> (fromMaybe False <$> o .:? "multi")
-----------------------------------------------------------------------------
data ReconnectPayload = ReconnectPayload EdgeId Connection
-----------------------------------------------------------------------------
instance FromJSON ReconnectPayload where
  parseJSON = withObject "ReconnectPayload" $ \o ->
    ReconnectPayload
      <$> o .: "edgeId"
      <*> o .: "connection"
-----------------------------------------------------------------------------
#ifdef WASM
-- The WASM linker has no @js-sources@ support, so the bridge is spliced
-- in as a JSFFI snippet and evaluated once, on the first store.
bridgeLoaded :: IORef Bool
{-# NOINLINE bridgeLoaded #-}
bridgeLoaded = unsafePerformIO (newIORef False)
#endif

-- | Create a JavaScript store over the flow's root element. Expects the
-- element to contain a pane (@.\<lib\>-flow__pane@) and a viewport
-- (@.xyflow__viewport@), as rendered by 'Miso.Flow.View.flowView'.
--
-- Requires @js\/miso-flow.js@ (it defines @globalThis.MisoFlow@). On
-- the GHCJS\/JS backends the library's @js-sources@ links it into the
-- compiled output; on WASM it is spliced in at compile time and
-- evaluated here on first use.
createFlowStore
  :: DOMRef
  -> StoreOptions
  -> Maybe (Connection -> Bool)
  -- ^ synchronous connection validator (XYHandle consults it while the
  -- pointer moves, so it cannot go through the action queue)
  -> BridgeCallbacks
  -> IO FlowStore
createFlowStore domRef options mValidate BridgeCallbacks {..} = do
#ifdef WASM
  loaded <- readIORef bridgeLoaded
  unless loaded $(evalFile "js/miso-flow.js")
  atomicWriteIORef bridgeLoaded True
#endif
  onViewport <- jsonCallback bcViewport
  onViewportStart <- jsonCallback bcViewportStart
  onViewportEnd <- jsonCallback bcViewportEnd
  onNodeChanges <- jsonCallback bcNodeChanges
  onConnectionUpdate <- jsonCallback bcConnectionUpdate
  onConnectStart <- jsonCallback bcConnectStart
  onConnect <- jsonCallback bcConnect
  onConnectEnd <- jsonCallback bcConnectEnd
  onNodeMouseDown <- jsonCallback (\(MouseDownPayload i multi) -> bcNodeMouseDown i multi)
  onUnselect <- asyncCallback1 (\_ -> bcUnselectAll)
  onPaneClick <- asyncCallback1 (\_ -> bcPaneClick)
  onNodeDragStart <- jsonCallback (\(IdPayload i) -> bcNodeDragStart i)
  onNodeDragStop <- jsonCallback (\(IdPayload i) -> bcNodeDragStop i)
  onResizeChanges <- jsonCallback bcResizeChanges
  onDimensions <- jsonCallback bcDimensions
  onSelectionRect <- jsonCallback bcSelectionRect
  onSelectionEnd <- jsonCallback bcSelectionEnd
  onReconnect <- jsonCallback (\(ReconnectPayload eid c) -> bcReconnect eid c)
  onResizeStart <- jsonCallback (\(IdPayload i) -> bcResizeStart i)
  onResizeEnd <- jsonCallback (\(IdPayload i) -> bcResizeEnd i)
  onMinimapClick <- jsonCallback bcMinimapClick
  onDeleteKey <- asyncCallback1 (\_ -> bcDeleteKey)
  validator <- case mValidate of
    Nothing -> pure Nothing
    Just f ->
      fmap Just $ syncCallback1' $ \payload -> do
        mStr <- fromJSVal payload
        toJSVal $ case decode =<< mStr of
          Just conn -> f conn
          Nothing -> True
  onError <- asyncCallback2 $ \code msg -> do
    mCode <- fromJSVal code
    mMsg <- fromJSVal msg
    case (mCode, mMsg) of
      (Just c, Just m) -> bcError c m
      _ -> pure ()
  callbacks <- createWith
    [ ("onViewport", onViewport)
    , ("onViewportStart", onViewportStart)
    , ("onViewportEnd", onViewportEnd)
    , ("onNodeChanges", onNodeChanges)
    , ("onConnectionUpdate", onConnectionUpdate)
    , ("onConnectStart", onConnectStart)
    , ("onConnect", onConnect)
    , ("onConnectEnd", onConnectEnd)
    , ("onNodeMouseDown", onNodeMouseDown)
    , ("onUnselectNodesAndEdges", onUnselect)
    , ("onPaneClick", onPaneClick)
    , ("onNodeDragStart", onNodeDragStart)
    , ("onNodeDragStop", onNodeDragStop)
    , ("onResizeChanges", onResizeChanges)
    , ("onDimensions", onDimensions)
    , ("onSelectionRect", onSelectionRect)
    , ("onSelectionEnd", onSelectionEnd)
    , ("onReconnect", onReconnect)
    , ("onResizeStart", onResizeStart)
    , ("onResizeEnd", onResizeEnd)
    , ("onMinimapClick", onMinimapClick)
    , ("onDeleteKey", onDeleteKey)
    , ("onError", onError)
    ]
  case validator of
    Nothing -> pure ()
    Just cb -> setField callbacks "isValidConnection" cb
  misoFlow <- jsg "MisoFlow"
  optionsVal <- toJSVal (encode options)
  callbacksVal <- toJSVal callbacks
  FlowStore <$> (misoFlow # "createStore" $ [ domRef, optionsVal, callbacksVal ])
-----------------------------------------------------------------------------
call0 :: FlowStore -> MisoString -> IO ()
call0 (FlowStore s) method = void (s # method $ ())
-----------------------------------------------------------------------------
callJSON :: ToJSON a => FlowStore -> MisoString -> a -> IO ()
callJSON (FlowStore s) method payload =
  void (s # method $ [ encode payload ])
-----------------------------------------------------------------------------
callJSON2 :: (ToJSON a, ToJSON b) => FlowStore -> MisoString -> a -> b -> IO ()
callJSON2 (FlowStore s) method p1 p2 =
  void (s # method $ (encode p1, encode p2))
-----------------------------------------------------------------------------
callRef :: FlowStore -> MisoString -> DOMRef -> IO ()
callRef (FlowStore s) method ref = void (s # method $ [ ref ])
-----------------------------------------------------------------------------
storeDestroy :: FlowStore -> IO ()
storeDestroy s = call0 s "destroy"
-----------------------------------------------------------------------------
storeSetNodes :: FlowStore -> [Node n] -> IO ()
storeSetNodes s = callJSON s "setNodes"
-----------------------------------------------------------------------------
storeSetEdges :: FlowStore -> [Edge e] -> IO ()
storeSetEdges s = callJSON s "setEdges"
-----------------------------------------------------------------------------
storeUpdateOptions :: FlowStore -> StoreOptions -> IO ()
storeUpdateOptions s = callJSON s "updateOptions"
-----------------------------------------------------------------------------
storeGetViewport :: FlowStore -> IO (Maybe Viewport)
storeGetViewport (FlowStore s) = do
  res <- s # "getViewport" $ ()
  mStr <- fromJSVal res
  pure (decode =<< mStr)
-----------------------------------------------------------------------------
storeSetViewport :: FlowStore -> Viewport -> ViewportHelperOptions -> IO ()
storeSetViewport s vp opts = callJSON2 s "setViewport" vp (helperOptions opts)
-----------------------------------------------------------------------------
storeSyncViewport :: FlowStore -> Viewport -> IO ()
storeSyncViewport s = callJSON s "syncViewport"
-----------------------------------------------------------------------------
helperOptions :: ViewportHelperOptions -> Value
helperOptions (ViewportHelperOptions duration interpolate) = object
  [ "duration" .= duration
  , "interpolate" .= interpolate
  ]
-----------------------------------------------------------------------------
storeZoomIn :: FlowStore -> ViewportHelperOptions -> IO ()
storeZoomIn s opts = callJSON s "zoomIn" (helperOptions opts)
-----------------------------------------------------------------------------
storeZoomOut :: FlowStore -> ViewportHelperOptions -> IO ()
storeZoomOut s opts = callJSON s "zoomOut" (helperOptions opts)
-----------------------------------------------------------------------------
storeZoomTo :: FlowStore -> Double -> ViewportHelperOptions -> IO ()
storeZoomTo (FlowStore s) zoom opts =
  void (s # "zoomTo" $ (zoom, encode (helperOptions opts)))
-----------------------------------------------------------------------------
storeScaleBy :: FlowStore -> Double -> ViewportHelperOptions -> IO ()
storeScaleBy (FlowStore s) factor opts =
  void (s # "scaleBy" $ (factor, encode (helperOptions opts)))
-----------------------------------------------------------------------------
storeSetCenter :: FlowStore -> Double -> Double -> SetCenterOptions -> IO ()
storeSetCenter (FlowStore s) x y (SetCenterOptions zoom opts) = do
  let ViewportHelperOptions duration interpolate = opts
  void $ s # "setCenter" $
    ( x
    , y
    , encode $ object
        [ "zoom" .= zoom
        , "duration" .= duration
        , "interpolate" .= interpolate
        ]
    )
-----------------------------------------------------------------------------
storeFitView :: FlowStore -> FitViewOptions -> IO ()
storeFitView s FitViewOptions {..} =
  callJSON s "fitView" $ object
    [ "padding" .= paddingValue fitViewPadding
    , "includeHiddenNodes" .= fitViewIncludeHiddenNodes
    , "minZoom" .= fitViewMinZoom
    , "maxZoom" .= fitViewMaxZoom
    , "duration" .= fitViewDuration
    , "interpolate" .= fitViewInterpolate
    , "nodes" .= (map (\i -> object [ "id" .= i ]) <$> fitViewNodes)
    ]
-----------------------------------------------------------------------------
paddingValue :: Padding -> Value
paddingValue = \case
  PaddingUniform p -> paddingWithUnitValue p
  PaddingSides top right bottom left px py -> object
    [ "top" .= (paddingWithUnitValue <$> top)
    , "right" .= (paddingWithUnitValue <$> right)
    , "bottom" .= (paddingWithUnitValue <$> bottom)
    , "left" .= (paddingWithUnitValue <$> left)
    , "x" .= (paddingWithUnitValue <$> px)
    , "y" .= (paddingWithUnitValue <$> py)
    ]
-----------------------------------------------------------------------------
paddingWithUnitValue :: PaddingWithUnit -> Value
paddingWithUnitValue = \case
  PaddingRatio r -> toJSON r
  PaddingPx v -> toJSON (jsShow v <> ("px" :: MisoString))
  PaddingPercent v -> toJSON (jsShow v <> ("%" :: MisoString))
-----------------------------------------------------------------------------
storeFitBounds :: FlowStore -> Rect -> FitBoundsOptions -> IO ()
storeFitBounds s bounds (FitBoundsOptions padding opts) =
  callJSON2 s "fitBounds" bounds $ object
    [ "padding" .= padding
    , "duration" .= vhoDuration opts
    , "interpolate" .= vhoInterpolate opts
    ]
-----------------------------------------------------------------------------
storePanBy :: FlowStore -> XYPosition -> IO ()
storePanBy s = callJSON s "panBy"
-----------------------------------------------------------------------------
storeObserveNode :: FlowStore -> DOMRef -> IO ()
storeObserveNode s = callRef s "observeNode"
-----------------------------------------------------------------------------
storeUnobserveNode :: FlowStore -> DOMRef -> IO ()
storeUnobserveNode s = callRef s "unobserveNode"
-----------------------------------------------------------------------------
storeRequestNodeMeasure :: FlowStore -> NodeId -> IO ()
storeRequestNodeMeasure (FlowStore s) nid =
  void (s # "requestNodeMeasure" $ [ nid ])
-----------------------------------------------------------------------------
storeAttachNodeDrag :: FlowStore -> DOMRef -> NodeId -> IO ()
storeAttachNodeDrag (FlowStore s) el nid = do
  nidVal <- toJSVal nid
  void (s # "attachNodeDrag" $ [ el, nidVal ])
-----------------------------------------------------------------------------
storeUpdateNodeDrag :: FlowStore -> DOMRef -> NodeId -> IO ()
storeUpdateNodeDrag (FlowStore s) el nid = do
  nidVal <- toJSVal nid
  void (s # "updateNodeDrag" $ [ el, nidVal ])
-----------------------------------------------------------------------------
storeDetachNodeDrag :: FlowStore -> NodeId -> IO ()
storeDetachNodeDrag (FlowStore s) nid =
  void (s # "detachNodeDrag" $ [ nid ])
-----------------------------------------------------------------------------
storeAttachHandle :: FlowStore -> DOMRef -> IO ()
storeAttachHandle s = callRef s "attachHandle"
-----------------------------------------------------------------------------
storeAttachReconnectAnchor :: FlowStore -> DOMRef -> IO ()
storeAttachReconnectAnchor s = callRef s "attachReconnectAnchor"
-----------------------------------------------------------------------------
storeAttachResizer :: FlowStore -> DOMRef -> NodeId -> Value -> IO ()
storeAttachResizer (FlowStore s) el nid params = do
  nidVal <- toJSVal nid
  paramsVal <- toJSVal (encode params)
  void (s # "attachResizer" $ [ el, nidVal, paramsVal ])
-----------------------------------------------------------------------------
storeUpdateResizer :: FlowStore -> NodeId -> DOMRef -> Value -> IO ()
storeUpdateResizer (FlowStore s) nid el params = do
  nidVal <- toJSVal nid
  paramsVal <- toJSVal (encode params)
  void (s # "updateResizer" $ [ nidVal, el, paramsVal ])
-----------------------------------------------------------------------------
storeDetachResizer :: FlowStore -> NodeId -> DOMRef -> IO ()
storeDetachResizer (FlowStore s) nid el = do
  nidVal <- toJSVal nid
  void (s # "detachResizer" $ [ nidVal, el ])
-----------------------------------------------------------------------------
storeAttachMinimap :: FlowStore -> DOMRef -> Value -> IO ()
storeAttachMinimap (FlowStore s) el params = do
  paramsVal <- toJSVal (encode params)
  void (s # "attachMinimap" $ [ el, paramsVal ])
-----------------------------------------------------------------------------
storeUpdateMinimap :: FlowStore -> Value -> IO ()
storeUpdateMinimap s params = callJSON s "updateMinimap" params
-----------------------------------------------------------------------------
