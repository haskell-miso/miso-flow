-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow
-- License     :  BSD3-style (see the file LICENSE)
--
-- Node-based UIs for miso, powered by xyflow. This module wraps the
-- pure ports ("Miso.Flow.Types", "Miso.Flow.Utils"), the view layer
-- ("Miso.Flow.View") and the JavaScript gesture bridge
-- ("Miso.Flow.Internal.Bridge") into an MVU component:
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents $
--   flowComponent defaultStoreOptions (text . nodeLabel) id myNodes myEdges
-- @
--
-- Requires @js\/miso-flow.js@ to be loaded on the page (it defines
-- @globalThis.MisoFlow@).
----------------------------------------------------------------------------
module Miso.Flow
  ( -- * Component
    flowComponent
    -- * Settings
  , FlowSettings (..)
  , defaultFlowSettings
    -- * Model
  , FlowModel
  , flowModel
  , flowNodes
  , flowEdges
  , flowViewport
  , flowConnection
  , flowOptions
    -- * Actions
  , FlowAction (..)
    -- * Update \/ view (for manual embedding)
  , updateFlow
  , updateFlowWith
  , viewFlow
  , flowHooks
  , sceneFromModel
    -- * Re-exports
  , StoreOptions (..)
  , defaultStoreOptions
  , module Miso.Flow.Types
  , module Miso.Flow.View
  , flowCSS
  , flowStyles
  , flowBaseCSS
  , flowBaseStyles
  , flowThemeCSS
  , flowThemeStyles
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (unless, when)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Effect (DOMRef, Effect, io_, withSink)
import           Miso.JSON (Value, object, (.=))
import           Miso.State (get, modify, put)
import           Miso.String (MisoString)
import           Miso.Types (Component (..), View, component)
-----------------------------------------------------------------------------
import           Miso.Flow.Constants (infiniteExtent)
import           Miso.Flow.Internal.Bridge
import           Miso.Flow.Style
  ( flowBaseCSS
  , flowBaseStyles
  , flowCSS
  , flowStyles
  , flowThemeCSS
  , flowThemeStyles
  )
import           Miso.Flow.Types
import           Miso.Flow.Utils.Edges (addEdge, reconnectEdge)
import           Miso.Flow.Utils.General (pointToRendererPoint)
import           Miso.Flow.Utils.Graph (getElementsToRemove, getNodesInside)
import           Miso.Flow.Utils.Store
  ( UpdateNodesOptions (..)
  , NodeMeasurement (..)
  , adoptUserNodes
  , applyMeasurement
  , defaultUpdateNodesOptions
  )
import           Miso.Flow.View
-----------------------------------------------------------------------------
-- | Attach work that arrived before the JavaScript store existed
-- (lifecycle hooks fire in creation order, so node \/ handle elements
-- can report in before 'FlowStoreCreated' lands). Never rendered.
data PendingAttach = PendingAttach
  { pendingNodes    :: [(NodeId, DOMRef)]
  , pendingHandles  :: [DOMRef]
  , pendingResizers :: [(NodeId, Value, DOMRef)]
  , pendingMinimaps :: [(MinimapConfig, DOMRef)]
  }
-----------------------------------------------------------------------------
-- | 'DOMRef's have no 'Eq'; compare by what accumulated instead. The
-- runtime only commits a model that differs from the previous one, so
-- queueing must register as a change. Pending only ever grows until the
-- store arrives (afterwards elements attach directly), so node ids plus
-- the handle count identify the queue.
instance Eq PendingAttach where
  PendingAttach ns hs rs ms == PendingAttach ns' hs' rs' ms' =
    map fst ns == map fst ns'
      && length hs == length hs'
      && map (\(i, v, _) -> (i, v)) rs == map (\(i, v, _) -> (i, v)) rs'
      && map (\(c, _) -> c) ms == map (\(c, _) -> c) ms'
-----------------------------------------------------------------------------
noPending :: PendingAttach
noPending = PendingAttach [] [] [] []
-----------------------------------------------------------------------------
-- | Behavioral settings that cannot live in the (JSON-serialized)
-- 'StoreOptions' or the ('Eq'-constrained) model.
newtype FlowSettings = FlowSettings
  { fsValidateConnection :: Maybe (Connection -> Bool)
    -- ^ consulted synchronously by the gesture system while a
    -- connection (or reconnection) is dragged; invalid targets render
    -- the connection line in the invalid style and refuse to connect
  }
-----------------------------------------------------------------------------
defaultFlowSettings :: FlowSettings
defaultFlowSettings = FlowSettings
  { fsValidateConnection = Nothing
  }
-----------------------------------------------------------------------------
-- | State of one flow: the user graph, its computed internals, and the
-- handle to the JavaScript store.
data FlowModel n e = FlowModel
  { fmNodes        :: [Node n]
  , fmEdges        :: [Edge e]
  , fmNodeLookup   :: NodeLookup n
  , fmParentLookup :: ParentLookup n
  , fmViewport     :: Viewport
  , fmConnection   :: ConnectionState n
  , fmStore        :: Maybe FlowStore
  , fmPending      :: PendingAttach
  , fmOptions      :: StoreOptions
  , fmDimensions   :: Dimensions
  , fmMinimap      :: Maybe MinimapConfig
    -- ^ set once a 'minimapView' reports in
  , fmSelectionRect :: Maybe Rect
    -- ^ in-progress selection box (container coordinates)
  } deriving Eq
-----------------------------------------------------------------------------
flowNodes :: FlowModel n e -> [Node n]
flowNodes = fmNodes
-----------------------------------------------------------------------------
flowEdges :: FlowModel n e -> [Edge e]
flowEdges = fmEdges
-----------------------------------------------------------------------------
flowViewport :: FlowModel n e -> Viewport
flowViewport = fmViewport
-----------------------------------------------------------------------------
flowConnection :: FlowModel n e -> ConnectionState n
flowConnection = fmConnection
-----------------------------------------------------------------------------
flowOptions :: FlowModel n e -> StoreOptions
flowOptions = fmOptions
-----------------------------------------------------------------------------
-- | Initial model from user nodes and edges.
flowModel :: Eq n => StoreOptions -> [Node n] -> [Edge e] -> FlowModel n e
flowModel options nodes edges = resync FlowModel
  { fmNodes = nodes
  , fmEdges = edges
  , fmNodeLookup = M.empty
  , fmParentLookup = M.empty
  , fmViewport = soDefaultViewport options
  , fmConnection = NoConnection
  , fmStore = Nothing
  , fmPending = noPending
  , fmOptions = options
  , fmDimensions = zeroDimensions
  , fmMinimap = Nothing
  , fmSelectionRect = Nothing
  }
-----------------------------------------------------------------------------
-- | Recompute the internal lookups from the user nodes.
resync :: Eq n => FlowModel n e -> FlowModel n e
resync m@FlowModel {..} =
  m { fmNodeLookup = nodeLookup, fmParentLookup = parentLookup }
  where
    (nodeLookup, parentLookup, _) =
      adoptUserNodes fmNodes fmNodeLookup defaultUpdateNodesOptions
        { unoNodeOrigin = soNodeOrigin fmOptions
        , unoNodeExtent = fromMaybe infiniteExtent (soNodeExtent fmOptions)
        , unoElevateNodesOnSelect = soElevateNodesOnSelect fmOptions
        , unoZIndexMode = soZIndexMode fmOptions
        }
-----------------------------------------------------------------------------
data FlowAction n e
  = FlowCreated DOMRef
    -- ^ container mounted: create the JavaScript store
  | FlowStoreCreated FlowStore
  | FlowBeforeDestroyed
  | FlowNodeCreated NodeId DOMRef
  | FlowNodeBeforeDestroyed NodeId DOMRef
  | FlowHandleCreated DOMRef
  | FlowViewportChanged Viewport
  | FlowNodeChangesReceived [WireNodeChange]
  | FlowConnectionChanged ConnectionStateWire
  | FlowConnected Connection
  | FlowNodeMouseDown NodeId Bool
    -- ^ id and whether a multi-selection modifier was held
  | FlowEdgeClicked EdgeId
  | FlowUnselected
  | FlowSelectionChanged Rect
  | FlowSelectionFinished
  | FlowRemoveNode NodeId
  | FlowReconnected EdgeId Connection
    -- ^ an edge anchor was dropped on a new valid handle
  | FlowResizeStarted NodeId
    -- ^ extension point: no-op in 'updateFlow'
  | FlowResizeFinished NodeId
    -- ^ extension point: no-op in 'updateFlow'
  | FlowMinimapClicked XYPosition
    -- ^ centers the viewport on the clicked flow position
  | FlowDeletePressed
    -- ^ Delete\/Backspace: remove the current selection
  | FlowDimensionsChanged Dimensions
  | FlowResizerCreated NodeId Value DOMRef
  | FlowResizerBeforeDestroyed NodeId DOMRef
  | FlowMinimapCreated MinimapConfig DOMRef
  | FlowEdgeAnchorCreated DOMRef
  | FlowSetNodes [Node n]
  | FlowSetEdges [Edge e]
  | FlowOptionsChanged StoreOptions
    -- ^ change store options at runtime (zoom limits, snapping, …)
  | FlowZoomIn
  | FlowZoomOut
  | FlowFitView
  | FlowErrorRaised MisoString MisoString
-----------------------------------------------------------------------------
-- | The standard wiring of VDOM lifecycle events into 'FlowAction's.
flowHooks :: FlowHooks (FlowAction n e)
flowHooks = FlowHooks
  { hookFlowCreated = FlowCreated
  , hookFlowBeforeDestroyed = FlowBeforeDestroyed
  , hookNodeCreated = FlowNodeCreated
  , hookNodeBeforeDestroyed = FlowNodeBeforeDestroyed
  , hookHandleCreated = FlowHandleCreated
  , hookEdgeClick = Just FlowEdgeClicked
  , hookNodeClick = Just FlowNodeMouseDown
  , hookResizerCreated = FlowResizerCreated
  , hookResizerBeforeDestroyed = FlowResizerBeforeDestroyed
  , hookMinimapCreated = FlowMinimapCreated
  , hookEdgeAnchorCreated = FlowEdgeAnchorCreated
  }
-----------------------------------------------------------------------------
-- | Bridge callbacks dispatching into the update loop.
flowCallbacks :: (FlowAction n e -> IO ()) -> BridgeCallbacks
flowCallbacks sink = emptyBridgeCallbacks
  { bcViewport = sink . FlowViewportChanged
  , bcNodeChanges = sink . FlowNodeChangesReceived
  , bcResizeChanges = sink . FlowNodeChangesReceived
  , bcConnectionUpdate = sink . FlowConnectionChanged
  , bcConnect = sink . FlowConnected
  , bcNodeMouseDown = \nid multi -> sink (FlowNodeMouseDown nid multi)
  , bcSelectionRect = sink . FlowSelectionChanged
  , bcSelectionEnd = \_ -> sink FlowSelectionFinished
  , bcReconnect = \eid c -> sink (FlowReconnected eid c)
  , bcResizeStart = sink . FlowResizeStarted
  , bcResizeEnd = sink . FlowResizeFinished
  , bcMinimapClick = sink . FlowMinimapClicked
  , bcDeleteKey = sink FlowDeletePressed
  , bcUnselectAll = sink FlowUnselected
  , bcPaneClick = sink FlowUnselected
  , bcDimensions = sink . FlowDimensionsChanged
  , bcError = \code msg -> sink (FlowErrorRaised code msg)
  }
-----------------------------------------------------------------------------
-- | 'updateFlowWith' with default settings.
updateFlow
  :: Eq n
  => FlowAction n e
  -> Effect ctx props (FlowModel n e) (FlowAction n e)
updateFlow = updateFlowWith defaultFlowSettings
-----------------------------------------------------------------------------
-- | Transition function; embed it directly if you are not using
-- 'flowComponent'.
updateFlowWith
  :: Eq n
  => FlowSettings
  -> FlowAction n e
  -> Effect ctx props (FlowModel n e) (FlowAction n e)
updateFlowWith settings = \case

  FlowCreated domRef -> do
    m <- get
    withSink $ \sink -> do
      store <- createFlowStore domRef (fmOptions m)
        (fsValidateConnection settings) (flowCallbacks sink)
      sink (FlowStoreCreated store)

  FlowStoreCreated store -> do
    m <- get
    let PendingAttach ns hs rs mms = fmPending m
    put m
      { fmStore = Just store
      , fmPending = noPending
      , fmMinimap = case mms of
          (c, _) : _ -> Just c
          [] -> fmMinimap m
      }
    io_ $ do
      storeSetNodes store (fmNodes m)
      storeSetEdges store (fmEdges m)
      mapM_ (attachNode store) (reverse ns)
      mapM_ (storeAttachHandle store) (reverse hs)
      mapM_ (\(nid, params, ref) -> storeAttachResizer store ref nid params) (reverse rs)
      mapM_ (\(c, ref) -> storeAttachMinimap store ref (minimapParams c 1)) (reverse mms)
    syncMinimap

  FlowBeforeDestroyed -> do
    m <- get
    put m { fmStore = Nothing, fmPending = noPending }
    mapM_ (io_ . storeDestroy) (fmStore m)

  FlowNodeCreated nid domRef -> do
    m <- get
    case fmStore m of
      Just store -> io_ (attachNode store (nid, domRef))
      Nothing -> put m
        { fmPending = (fmPending m)
            { pendingNodes = (nid, domRef) : pendingNodes (fmPending m) }
        }

  FlowNodeBeforeDestroyed nid domRef -> do
    m <- get
    mapM_
      (\store -> io_ $ do
          storeUnobserveNode store domRef
          storeDetachNodeDrag store nid)
      (fmStore m)

  FlowHandleCreated domRef -> do
    m <- get
    case fmStore m of
      Just store -> io_ (storeAttachHandle store domRef)
      Nothing -> put m
        { fmPending = (fmPending m)
            { pendingHandles = domRef : pendingHandles (fmPending m) }
        }

  FlowViewportChanged viewport -> do
    modify $ \m -> m { fmViewport = viewport }
    syncMinimap

  FlowNodeChangesReceived changes -> do
    modify $ \m -> resync (foldl (flip applyWireChange) m changes)
    -- keep the JavaScript lookup in step (resize changes and removals
    -- are not applied on the JS side by the bridge itself)
    when (any needsJsSync changes) pushGraph
    syncMinimap

  FlowConnectionChanged wire ->
    -- the gesture system reports @to@ / @pointer@ in pane (screen)
    -- coordinates while @from@ is already in flow coordinates; convert
    -- before rendering, as the framework packages do in useConnection
    modify $ \m ->
      let toFlow p = pointToRendererPoint p (fmViewport m) False (SnapGrid 1 1)
          wire' = wire
            { cwTo = toFlow <$> cwTo wire
            , cwPointer = toFlow <$> cwPointer wire
            }
      in m { fmConnection =
               wireConnectionState (`M.lookup` fmNodeLookup m) wire'
           }

  FlowConnected c -> do
    modify $ \m -> m { fmEdges = addEdge c (fmEdges m) }
    pushEdges

  FlowNodeMouseDown nid multi
    -- multi-modifier: toggle this node, keep the rest
    | multi -> do
        modify $ \m -> resync m
          { fmNodes =
              [ if nodeId n == nid
                  then n { nodeSelected = not (nodeSelected n) }
                  else n
              | n <- fmNodes m
              ]
          }
        pushGraph
    | otherwise -> do
        m <- get
        -- mousedown on an already-selected node keeps the selection
        -- (that is how a multi-selection gets dragged as a group)
        let alreadySelected =
              any (\n -> nodeId n == nid && nodeSelected n) (fmNodes m)
        unless alreadySelected $ do
          put $ resync m
            { fmNodes =
                [ n { nodeSelected = nodeId n == nid } | n <- fmNodes m ]
            , fmEdges = [ e { edgeSelected = False } | e <- fmEdges m ]
            }
          pushGraph

  FlowEdgeClicked eid -> do
    modify $ \m -> resync m
      { fmNodes = [ n { nodeSelected = False } | n <- fmNodes m ]
      , fmEdges = [ e { edgeSelected = edgeId e == eid } | e <- fmEdges m ]
      }
    pushGraph

  FlowUnselected -> do
    m <- get
    let selected =
          any nodeSelected (fmNodes m) || any edgeSelected (fmEdges m)
    when selected $ do
      put $ resync m
        { fmNodes = [ n { nodeSelected = False } | n <- fmNodes m ]
        , fmEdges = [ e { edgeSelected = False } | e <- fmEdges m ]
        }
      pushGraph

  FlowSelectionChanged rect -> do
    modify $ \m ->
      let inside =
            S.fromList
              [ nodeId (internalUserNode n)
              | n <- getNodesInside (fmNodeLookup m) rect (fmViewport m) False False
              ]
          nodes = [ n { nodeSelected = nodeId n `S.member` inside } | n <- fmNodes m ]
      in resync m
           { fmSelectionRect = Just rect
           , fmNodes = nodes
           , fmEdges =
               [ e { edgeSelected =
                       edgeSource e `S.member` inside
                         && edgeTarget e `S.member` inside }
               | e <- fmEdges m
               ]
           }
    pushGraph

  FlowSelectionFinished ->
    modify $ \m -> m { fmSelectionRect = Nothing }

  FlowRemoveNode nid -> do
    modify $ \m -> resync m
      { fmNodes = [ n | n <- fmNodes m, nodeId n /= nid ]
      , fmEdges =
          [ e | e <- fmEdges m, edgeSource e /= nid, edgeTarget e /= nid ]
      }
    pushGraph
    syncMinimap

  FlowReconnected eid c -> do
    modify $ \m ->
      case [ e | e <- fmEdges m, edgeId e == eid ] of
        (old : _) -> m { fmEdges = reconnectEdge old c (fmEdges m) }
        [] -> m
    pushEdges

  FlowResizeStarted _ -> pure ()

  FlowResizeFinished _ -> pure ()

  FlowMinimapClicked (XYPosition x y) ->
    onStore (\s -> storeSetCenter s x y defaultSetCenterOptions)

  FlowDeletePressed -> do
    m <- get
    let selectedNodes = [ nodeId n | n <- fmNodes m, nodeSelected n ]
        selectedEdges = [ edgeId e | e <- fmEdges m, edgeSelected e ]
        (goneNodes, goneEdges) =
          getElementsToRemove selectedNodes selectedEdges (fmNodes m) (fmEdges m)
        goneNodeIds = S.fromList (map nodeId goneNodes)
        goneEdgeIds = S.fromList (map edgeId goneEdges)
    when (not (null goneNodes) || not (null goneEdges)) $ do
      put $ resync m
        { fmNodes = [ n | n <- fmNodes m, nodeId n `S.notMember` goneNodeIds ]
        , fmEdges =
            [ e
            | e <- fmEdges m
            , edgeId e `S.notMember` goneEdgeIds
            , edgeSource e `S.notMember` goneNodeIds
            , edgeTarget e `S.notMember` goneNodeIds
            ]
        }
      pushGraph
      syncMinimap

  FlowEdgeAnchorCreated domRef -> do
    m <- get
    -- anchors only render on selected edges, which implies the store
    -- already exists; if it somehow doesn't, the anchor is inert
    mapM_ (\store -> io_ (storeAttachReconnectAnchor store domRef)) (fmStore m)

  FlowDimensionsChanged dims -> do
    modify $ \m -> m { fmDimensions = dims }
    syncMinimap

  FlowResizerCreated nid params domRef -> do
    m <- get
    case fmStore m of
      Just store -> io_ (storeAttachResizer store domRef nid params)
      Nothing -> put m
        { fmPending = (fmPending m)
            { pendingResizers = (nid, params, domRef) : pendingResizers (fmPending m) }
        }

  FlowResizerBeforeDestroyed nid domRef -> do
    m <- get
    mapM_ (\store -> io_ (storeDetachResizer store nid domRef)) (fmStore m)

  FlowMinimapCreated cfg domRef -> do
    m <- get
    case fmStore m of
      Just store -> do
        put m { fmMinimap = Just cfg }
        io_ (storeAttachMinimap store domRef (minimapParams cfg 1))
        syncMinimap
      Nothing -> put m
        { fmMinimap = Just cfg
        , fmPending = (fmPending m)
            { pendingMinimaps = (cfg, domRef) : pendingMinimaps (fmPending m) }
        }

  FlowSetNodes nodes -> do
    modify $ \m -> resync m { fmNodes = nodes }
    pushGraph
    syncMinimap

  FlowSetEdges edges -> do
    modify $ \m -> m { fmEdges = edges }
    pushEdges

  FlowOptionsChanged options -> do
    modify $ \m -> resync m { fmOptions = options }
    m <- get
    mapM_ (\store -> io_ (storeUpdateOptions store options)) (fmStore m)

  FlowZoomIn -> onStore (`storeZoomIn` defaultViewportHelperOptions)

  FlowZoomOut -> onStore (`storeZoomOut` defaultViewportHelperOptions)

  FlowFitView -> onStore (`storeFitView` defaultFitViewOptions)

  FlowErrorRaised _ _ -> pure ()

  where
    onStore f = get >>= mapM_ (io_ . f) . fmStore
    pushGraph = do
      m <- get
      mapM_ (\s -> io_ (storeSetNodes s (fmNodes m) >> storeSetEdges s (fmEdges m))) (fmStore m)
    pushEdges = do
      m <- get
      mapM_ (\s -> io_ (storeSetEdges s (fmEdges m))) (fmStore m)
    syncMinimap = do
      m <- get
      case (fmStore m, fmMinimap m) of
        (Just store, Just cfg) ->
          io_ $ storeUpdateMinimap store $ minimapParams cfg $
            minimapViewScaleFor cfg (fmDimensions m) (fmViewport m) (fmNodeLookup m)
        _ -> pure ()
-----------------------------------------------------------------------------
-- | XYMinimap update parameters for a config and view scale.
minimapParams :: MinimapConfig -> Double -> Value
minimapParams MinimapConfig {..} viewScale = object
  [ "viewScale" .= viewScale
  , "inversePan" .= mmInversePan
  , "zoomStep" .= mmZoomStep
  , "pannable" .= mmPannable
  , "zoomable" .= mmZoomable
  ]
-----------------------------------------------------------------------------
attachNode :: FlowStore -> (NodeId, DOMRef) -> IO ()
attachNode store (nid, domRef) = do
  storeObserveNode store domRef
  storeAttachNodeDrag store domRef nid
-----------------------------------------------------------------------------
-- | Which wire changes leave the JavaScript store's own lookup stale.
needsJsSync :: WireNodeChange -> Bool
needsJsSync = \case
  WirePositionChange {} -> False -- XYDrag updates the JS lookup itself
  WireDimensionChange _ _ resizing setAttrs _ _ ->
    resizing == Just True || setAttrs /= SetAttributesNone
  WireSelectionChange {} -> True
  WireRemoveChange {} -> True
-----------------------------------------------------------------------------
-- | Fold one JS-reported change into the model (the user nodes carry
-- @measured@, like in xyflow; 'resync' then rebuilds the lookup and
-- 'applyMeasurement' merges the DOM-measured handle bounds).
applyWireChange :: WireNodeChange -> FlowModel n e -> FlowModel n e
applyWireChange change m = case change of
  WirePositionChange nid mPosition _ mDragging ->
    onNode nid $ \n -> n
      { nodePosition = fromMaybe (nodePosition n) mPosition
      , nodeDragging = fromMaybe (nodeDragging n) mDragging
      }
  WireDimensionChange nid mDims mResizing setAttrs mHandleBounds mPosAbs ->
    let withDims = onNode nid $ \n -> n
          { nodeMeasured =
              maybe (nodeMeasured n)
                (\(Dimensions w h) -> Just (Measured (Just w) (Just h)))
                mDims
          , nodeResizing = fromMaybe (nodeResizing n) mResizing
          , nodeWidth = case (setAttrs, mDims) of
              (SetAttributesWidth, Just d) -> Just (dimensionsWidth d)
              (SetAttributesBoth, Just d) -> Just (dimensionsWidth d)
              _ -> nodeWidth n
          , nodeHeight = case (setAttrs, mDims) of
              (SetAttributesHeight, Just d) -> Just (dimensionsHeight d)
              (SetAttributesBoth, Just d) -> Just (dimensionsHeight d)
              _ -> nodeHeight n
          }
    in case mDims of
         Nothing -> withDims
         Just dims -> withDims
           { fmNodeLookup =
               applyMeasurement
                 [ NodeMeasurement
                     { nmId = nid
                     , nmDimensions = dims
                     , nmHandleBounds = mHandleBounds
                     , nmPositionAbsolute = mPosAbs
                     }
                 ]
                 (fmNodeLookup withDims)
           }
  WireSelectionChange nid selected ->
    onNode nid $ \n -> n { nodeSelected = selected }
  WireRemoveChange nid ->
    m { fmNodes = [ n | n <- fmNodes m, nodeId n /= nid ]
      , fmEdges =
          [ e | e <- fmEdges m, edgeSource e /= nid, edgeTarget e /= nid ]
      }
  where
    onNode nid f =
      m { fmNodes =
            [ if nodeId n == nid then f n else n | n <- fmNodes m ]
        }
-----------------------------------------------------------------------------
-- | The scene 'flowView' draws for a model.
sceneFromModel :: FlowModel n e -> FlowScene n e
sceneFromModel FlowModel {..} = FlowScene
  { sceneNodes = fmNodes
  , sceneNodeLookup = fmNodeLookup
  , sceneEdges = fmEdges
  , sceneViewport = fmViewport
  , sceneConnection = fmConnection
  , sceneDimensions = fmDimensions
  , sceneSelectionRect = fmSelectionRect
  , sceneOptions = fmOptions
  }
-----------------------------------------------------------------------------
-- | Draw a model with a view config (see 'flowViewConfig').
viewFlow
  :: FlowViewConfig ctx n e (FlowModel n e) (FlowAction n e)
  -> FlowModel n e
  -> View ctx (FlowModel n e) (FlowAction n e)
viewFlow cfg = flowView cfg . sceneFromModel
-----------------------------------------------------------------------------
-- | A ready-to-mount flow. The view config passed to the customization
-- function is pre-wired: hooks, flow id and connection mode match the
-- store options, nodes render the given label view.
flowComponent
  :: Eq n
  => StoreOptions
  -> FlowSettings
  -> (n -> View ctx (FlowModel n e) (FlowAction n e))
  -- ^ node label
  -> (FlowViewConfig ctx n e (FlowModel n e) (FlowAction n e)
      -> FlowViewConfig ctx n e (FlowModel n e) (FlowAction n e))
  -- ^ view customization ('id' for the defaults)
  -> [Node n]
  -> [Edge e]
  -> Component ctx props (FlowModel n e) (FlowAction n e)
flowComponent options settings label customize nodes edges =
  (component (flowModel options nodes edges) (updateFlowWith settings) view)
    { styles = [ flowCSS ] }
  where
    cfg = customize (flowViewConfig flowHooks label)
      { fvcFlowId = soFlowId options
      , fvcConnectionMode = soConnectionMode options
      }
    view _ctx _props = viewFlow cfg
-----------------------------------------------------------------------------
