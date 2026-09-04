-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.View
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure views rendering the DOM contract that the miso-flow JavaScript
-- bridge (and the imperative @\@xyflow\/system@ modules behind it)
-- expects:
--
-- @
-- div.miso-flow[.dark]
--   svg.miso-flow__background          (optional dot grid)
--   div.miso-flow__pane
--     div.miso-flow__viewport.xyflow__viewport   (viewport transform)
--       svg.miso-flow__edges           (edge paths + marker defs)
--       div.miso-flow__nodes
--         div.miso-flow__node[data-id] (measured + dragged by the bridge)
--           …user content…
--           div.miso-flow__handle…     (connection handles)
--       svg.miso-flow__connectionline  (while connecting)
--   div.miso-flow__panel…              (controls, custom panels)
-- @
--
-- The selector contract: the bridge looks up @.miso-flow__pane@ and
-- @.xyflow__viewport@, measures handles via the @source@ \/ @target@
-- classes and @data-handleid@ \/ @data-handlepos@, and resolves
-- connections over @data-nodeid@ \/ @data-id@ and the @connectable@ \/
-- @connectablestart@ \/ @connectableend@ classes.
----------------------------------------------------------------------------
module Miso.Flow.View
  ( -- * Lifecycle hooks
    FlowHooks (..)
    -- * Configuration
  , FlowViewConfig (..)
  , flowViewConfig
  , NodeContentRenderer
    -- * Scene
  , FlowScene (..)
    -- * Views
  , flowView
  , defaultNodeContent
  , handleView
  , panelView
  , controlsView
    -- * Minimap
  , MinimapConfig (..)
  , defaultMinimapConfig
  , minimapView
  , minimapViewScaleFor
    -- * Node resizer
  , ResizerConfig (..)
  , defaultResizerConfig
  , nodeResizerView
    -- * Node toolbar
  , ToolbarConfig (..)
  , defaultToolbarConfig
  , nodeToolbarView
  , edgeToolbarView
    -- * Edge paths
  , edgePathFor
  , edgePositionFor
  , connectionPathFor
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.CSS (style_)
import           Miso.Effect (DOMRef)
import           Miso.JSON (Value, object, withObject, (.:), (.=))
import           Miso.Event
  ( Decoder (..)
  , DecodeTarget (DecodeTarget)
  , on
  , onBeforeDestroyed
  , onBeforeDestroyedWith
  , onCreatedWith
  )
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Property (key_, textProp)
import           Miso.String (MisoString)
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as SP
import           Miso.Types (Attribute, View, text)
-----------------------------------------------------------------------------
import           Miso.Flow.Internal.Bridge (StoreOptions)
import           Miso.Flow.Internal.JSNum (jsShow)
import           Miso.Flow.Types
import           Miso.Flow.Utils.Edges
import           Miso.Flow.Utils.General
  ( getBoundsOfRects
  , getInternalNodeDimensions
  , internalNodeToRect
  )
import           Miso.Flow.Utils.Graph (getInternalNodesBounds)
import           Miso.Flow.Utils.Toolbar
  ( AlignX (AlignXCenter)
  , AlignY (AlignYCenter)
  , getEdgeToolbarTransform
  , getNodeToolbarTransform
  )
import           Miso.Flow.Utils.Marker (createMarkerIds, getMarkerId)
-----------------------------------------------------------------------------
-- | Actions dispatched from VDOM lifecycle events; the component layer
-- uses these to create the JavaScript store and attach gestures to the
-- elements as they appear.
data FlowHooks action = FlowHooks
  { hookFlowCreated :: DOMRef -> action
    -- ^ container inserted: create the store
  , hookFlowBeforeDestroyed :: action
    -- ^ container about to leave: destroy the store
  , hookNodeCreated :: NodeId -> DOMRef -> action
    -- ^ node element inserted: observe + attach drag
  , hookNodeBeforeDestroyed :: NodeId -> DOMRef -> action
    -- ^ node element about to leave: unobserve + detach drag
  , hookHandleCreated :: DOMRef -> action
    -- ^ handle element inserted: attach connection gestures
  , hookEdgeClick :: Maybe (EdgeId -> action)
    -- ^ optional: click on an edge's interaction path
  , hookNodeClick :: Maybe (NodeId -> Bool -> action)
    -- ^ optional: plain click on a node; the 'Bool' is 'True' when a
    -- multi-selection modifier (shift\/meta\/ctrl) was held. Drags don't
    -- reach it — d3-drag suppresses the click after a real drag.
  , hookResizerCreated :: NodeId -> Value -> DOMRef -> action
    -- ^ resize control inserted (the 'Value' carries the XYResizer
    -- parameters built by 'nodeResizerView')
  , hookResizerBeforeDestroyed :: NodeId -> DOMRef -> action
    -- ^ resize control about to leave
  , hookMinimapCreated :: MinimapConfig -> DOMRef -> action
    -- ^ minimap svg inserted: attach XYMinimap pan\/zoom
  , hookEdgeAnchorCreated :: DOMRef -> action
    -- ^ edge reconnect anchor inserted: attach the reconnect gesture
  }
-----------------------------------------------------------------------------
-- | Content rendered /inside/ the node wrapper element (the wrapper
-- itself — positioning, classes, lifecycle — is owned by 'flowView').
type NodeContentRenderer ctx n e model action
  = FlowViewConfig ctx n e model action
  -> Node n
  -> [View ctx model action]
-----------------------------------------------------------------------------
data FlowViewConfig ctx n e model action = FlowViewConfig
  { fvcFlowId :: MisoString
    -- ^ must match 'Miso.Flow.Internal.Bridge.soFlowId'
  , fvcDark :: Bool
    -- ^ add the @dark@ class (see "Miso.Flow.Style")
  , fvcConnectionMode :: ConnectionMode
    -- ^ must match 'Miso.Flow.Internal.Bridge.soConnectionMode'
  , fvcConnectionLineType :: ConnectionLineType
    -- ^ path style of the in-progress connection line
  , fvcShowBackground :: Bool
    -- ^ render the dot-grid background
  , fvcNodeContent :: NodeContentRenderer ctx n e model action
  , fvcAttrs :: [Attribute model action]
    -- ^ extra attributes for the container (id, inline size, …)
  , fvcChildren :: FlowScene n e -> [View ctx model action]
    -- ^ overlays for the current scene: panels, controls, minimap, …
  , fvcViewportChildren :: FlowScene n e -> [View ctx model action]
    -- ^ overlays rendered /inside/ the transformed viewport (flow
    -- coordinates): edge toolbars, custom labels, …
  , fvcEdgesReconnectable :: Bool
    -- ^ render reconnect anchors on selected edges
  , fvcHooks :: FlowHooks action
  }
-----------------------------------------------------------------------------
-- | Config with xyflow-like defaults; nodes render their 'nodeData'
-- through the given label view.
flowViewConfig
  :: FlowHooks action
  -> (n -> View ctx model action)
  -- ^ node label
  -> FlowViewConfig ctx n e model action
flowViewConfig hooks label = FlowViewConfig
  { fvcFlowId = "1"
  , fvcDark = False
  , fvcConnectionMode = ConnectionModeStrict
  , fvcConnectionLineType = ConnectionLineBezier
  , fvcShowBackground = True
  , fvcNodeContent = defaultNodeContent (label . nodeData)
  , fvcAttrs = []
  , fvcChildren = const []
  , fvcViewportChildren = const []
  , fvcEdgesReconnectable = False
  , fvcHooks = hooks
  }
-----------------------------------------------------------------------------
-- | Everything 'flowView' draws in one frame.
data FlowScene n e = FlowScene
  { sceneNodes :: [Node n]
    -- ^ user nodes, in render order
  , sceneNodeLookup :: NodeLookup n
    -- ^ internals for 'sceneNodes' (absolute positions, z, measurements)
  , sceneEdges :: [Edge e]
  , sceneViewport :: Viewport
  , sceneConnection :: ConnectionState n
    -- ^ in-progress connection, if any
  , sceneDimensions :: Dimensions
    -- ^ container size as reported by the bridge (drives the minimap)
  , sceneSelectionRect :: Maybe Rect
    -- ^ in-progress selection box, in container coordinates
  , sceneOptions :: StoreOptions
    -- ^ current store options (snapping, zoom limits, …)
  }
-----------------------------------------------------------------------------
-- | The full flow container.
flowView
  :: FlowViewConfig ctx n e model action
  -> FlowScene n e
  -> View ctx model action
flowView cfg@FlowViewConfig {..} scene@FlowScene {..} =
  H.div_
    ( P.classes_ ("miso-flow" : [ "dark" | fvcDark ])
    : onCreatedWith (hookFlowCreated fvcHooks)
    : onBeforeDestroyed (hookFlowBeforeDestroyed fvcHooks)
    : fvcAttrs
    )
    ( [ backgroundView fvcFlowId sceneViewport | fvcShowBackground ]
   <> [ H.div_
          [ P.class_ "miso-flow__pane" ]
          ( H.div_
              [ P.classes_ [ "miso-flow__viewport", "xyflow__viewport" ]
              , style_ [ ("transform", viewportTransform sceneViewport) ]
              ]
              ( [ edgesView cfg scene
                , H.div_
                    [ P.class_ "miso-flow__nodes" ]
                    (map (nodeView cfg scene) sceneNodes)
                ]
             <> connectionLineView cfg sceneConnection
             <> fvcViewportChildren scene
              )
          : selectionRectView sceneSelectionRect
          )
      ]
   <> fvcChildren scene
    )
-----------------------------------------------------------------------------
viewportTransform :: Viewport -> MisoString
viewportTransform Viewport {..} = mconcat
  [ "translate(", jsShow viewportX, "px, ", jsShow viewportY, "px) "
  , "scale(", jsShow viewportZoom, ")"
  ]
-----------------------------------------------------------------------------
-- * Nodes
-----------------------------------------------------------------------------
nodeView
  :: FlowViewConfig ctx n e model action
  -> FlowScene n e
  -> Node n
  -> View ctx model action
nodeView cfg@FlowViewConfig {..} FlowScene {..} n
  | nodeHidden n =
      H.div_
        [ key_ (nodeId n)
        , P.class_ "miso-flow__node"
        , textProp "data-id" (nodeId n)
        , P.hidden_ True
        ]
        []
  | otherwise =
      H.div_
        ( [ key_ (nodeId n)
          , P.classes_ $ catMaybes
              [ Just "miso-flow__node"
              , ("miso-flow__node-" <>) <$> nodeType n
              , justWhen (nodeSelected n) "selected"
              , justWhen (nodeDragging n) "dragging"
              , justWhen draggable "nopan"
              ]
          , textProp "data-id" (nodeId n)
            -- always set explicitly: miso clears removed boolean
            -- properties via setAttribute(name, ''), which would leave
            -- a previously hidden node hidden
          , P.hidden_ False
          , style_ nodeStyles
          , onCreatedWith (hookNodeCreated fvcHooks (nodeId n))
          , onBeforeDestroyedWith (hookNodeBeforeDestroyed fvcHooks (nodeId n))
          ]
       <> [ on "click" multiModifierDecoder (\multi _ _ -> click (nodeId n) multi)
          | fromMaybe True (nodeSelectable n)
          , Just click <- [ hookNodeClick fvcHooks ]
          ]
        )
        (fvcNodeContent cfg n)
  where
    draggable = fromMaybe True (nodeDraggable n)
    internal = M.lookup (nodeId n) sceneNodeLookup
    positionAbsolute = maybe (nodePosition n) internalPositionAbsolute internal
    measured = maybe False (isJust . measuredToDimensions . internalMeasured) internal
    zIndex = maybe 0 internalZ internal
    nodeStyles = catMaybes
      [ Just
          ( "transform"
          , mconcat
              [ "translate(", jsShow (xyX positionAbsolute), "px, "
              , jsShow (xyY positionAbsolute), "px)"
              ]
          )
      , Just ("z-index", jsShow zIndex)
      , (\w -> ("width", jsShow w <> "px")) <$> nodeWidth n
      , (\h -> ("height", jsShow h <> "px")) <$> nodeHeight n
        -- hide until the bridge has measured it, to avoid a (0,0) flash
      , justWhen (not measured) ("visibility", "hidden")
      ]
-----------------------------------------------------------------------------
justWhen :: Bool -> a -> Maybe a
justWhen b a = if b then Just a else Nothing
-----------------------------------------------------------------------------
-- | Default node content: a target handle, the label, a source handle.
defaultNodeContent
  :: (Node n -> View ctx model action)
  -- ^ label
  -> NodeContentRenderer ctx n e model action
defaultNodeContent label cfg n =
  [ handleView cfg n TargetHandle (fromMaybe PositionTop (nodeTargetPosition n)) Nothing
  , H.div_ [ P.class_ "miso-flow__node-default" ] [ label n ]
  , handleView cfg n SourceHandle (fromMaybe PositionBottom (nodeSourcePosition n)) Nothing
  ]
-----------------------------------------------------------------------------
-- | A connection handle, carrying all attributes the gesture system
-- reads ('data-id' mirrors the TS template including its @null@ for a
-- missing handle id).
handleView
  :: FlowViewConfig ctx n e model action
  -> Node n
  -> HandleType
  -> Position
  -> Maybe MisoString
  -- ^ handle id (for multi-handle nodes)
  -> View ctx model action
handleView FlowViewConfig {..} n handleType position handleId =
  H.div_
    [ P.classes_ $
        [ "miso-flow__handle"
        , "miso-flow__handle-" <> positionToText position
        , handleTypeToText handleType
        , "nodrag"
        , "nopan"
        ]
     <> (if connectable then [ "connectable", "connectablestart", "connectableend" ] else [])
    , textProp "data-id" $ mconcat
        [ fvcFlowId, "-", nodeId n, "-"
        , fromMaybe "null" handleId, "-"
        , handleTypeToText handleType
        ]
    , textProp "data-nodeid" (nodeId n)
    , textProp "data-handlepos" (positionToText position)
    , maybe (P.classes_ []) (textProp "data-handleid") handleId
    , onCreatedWith (hookHandleCreated fvcHooks)
    ]
    []
  where
    connectable = fromMaybe True (nodeConnectable n)
-----------------------------------------------------------------------------
-- * Edges
-----------------------------------------------------------------------------
edgesView
  :: FlowViewConfig ctx n e model action
  -> FlowScene n e
  -> View ctx model action
edgesView cfg FlowScene {..} =
  S.svg_
    [ P.class_ "miso-flow__edges" ]
    ( markerDefs (fvcFlowId cfg) sceneEdges
    : map (edgeView cfg sceneNodeLookup) (filter visible sceneEdges)
    )
  where
    hiddenNode nid =
      maybe False (nodeHidden . internalUserNode) (M.lookup nid sceneNodeLookup)
    visible e =
      not (edgeHidden e)
        && not (hiddenNode (edgeSource e))
        && not (hiddenNode (edgeTarget e))
-----------------------------------------------------------------------------
edgeView
  :: FlowViewConfig ctx n e model action
  -> NodeLookup n
  -> Edge e
  -> View ctx model action
edgeView FlowViewConfig {..} nodeLookup e =
  S.g_
    [ key_ (edgeId e)
    , P.classes_ $ catMaybes
        [ Just "miso-flow__edge"
        , ("miso-flow__edge-" <>) <$> edgeType e
        , justWhen (edgeSelected e) "selected"
        , justWhen (edgeAnimated e) "animated"
        ]
    ]
    ( case edgePathFor fvcConnectionMode nodeLookup e of
        Nothing -> []
        Just ep ->
          [ S.path_
              ( [ P.class_ "miso-flow__edge-path"
                , SP.d_ (edgePath ep)
                ]
             <> markerAttr SP.markerStart_ (edgeMarkerStart e)
             <> markerAttr SP.markerEnd_ (edgeMarkerEnd e)
              )
          , S.path_
              ( [ P.class_ "miso-flow__edge-interaction"
                , SP.d_ (edgePath ep)
                , SP.strokeWidth_ (jsShow interactionWidth)
                ]
             <> [ H.onClick (click (edgeId e))
                | Just click <- [ hookEdgeClick fvcHooks ]
                ]
              )
          ]
          <> ( case edgePositionFor fvcConnectionMode nodeLookup e of
                 Just epos | reconnectable ->
                   [ edgeAnchor "source" (epSourceX epos) (epSourceY epos) (epSourcePosition epos)
                   , edgeAnchor "target" (epTargetX epos) (epTargetY epos) (epTargetPosition epos)
                   ]
                 _ -> []
             )
    )
  where
    interactionWidth = fromMaybe 20 (edgeInteractionWidth e)
    reconnectable = fvcEdgesReconnectable && edgeSelected e
    anchorRadius = 10 :: Double
    -- port of the framework packages' EdgeAnchor: a circle shifted
    -- towards the outside of the handle
    edgeAnchor anchorType cx cy position =
      S.circle_
        [ P.classes_
            [ "miso-flow__edge-anchor"
            , "miso-flow__edge-anchor-" <> anchorType
            , "nodrag"
            , "nopan"
            ]
        , textProp "data-edgeid" (edgeId e)
        , textProp "data-anchortype" anchorType
        , SP.cx_ (jsShow (shiftAxis cx PositionLeft PositionRight position))
        , SP.cy_ (jsShow (shiftAxis cy PositionTop PositionBottom position))
        , SP.r_ (jsShow anchorRadius)
        , onCreatedWith (hookEdgeAnchorCreated fvcHooks)
        ]
    shiftAxis v neg pos position
      | position == neg = v - anchorRadius
      | position == pos = v + anchorRadius
      | otherwise = v
    markerAttr attr = \case
      Nothing -> []
      Just m -> [ attr ("url(#" <> getMarkerId (Just m) (Just fvcFlowId) <> ")") ]
-----------------------------------------------------------------------------
-- * Node toolbar
-----------------------------------------------------------------------------
data ToolbarConfig = ToolbarConfig
  { tbPosition :: !Position
  , tbOffset :: !Double
  , tbAlign :: !Align
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
defaultToolbarConfig :: ToolbarConfig
defaultToolbarConfig = ToolbarConfig
  { tbPosition = PositionTop
  , tbOffset = 10
  , tbAlign = AlignCenter
  }
-----------------------------------------------------------------------------
-- | A toolbar floating next to a node, in container coordinates (it
-- does not scale with the zoom). Include it via 'fvcChildren':
--
-- @
-- fvcChildren = \\scene ->
--   [ v
--   | n <- sceneNodes scene, nodeSelected n
--   , v <- nodeToolbarView defaultToolbarConfig scene (nodeId n) [ … ]
--   ]
-- @
nodeToolbarView
  :: ToolbarConfig
  -> FlowScene n e
  -> NodeId
  -> [View ctx model action]
  -- ^ toolbar content
  -> [View ctx model action]
nodeToolbarView ToolbarConfig {..} FlowScene {..} nid content =
  case M.lookup nid sceneNodeLookup of
    Just internal
      | Just _ <- measuredToDimensions (internalMeasured internal) ->
          [ H.div_
              [ key_ ("toolbar-" <> nid)
              , P.class_ "miso-flow__node-toolbar"
              , style_
                  [ ( "transform"
                    , getNodeToolbarTransform
                        (internalNodeToRect internal)
                        sceneViewport tbPosition tbOffset tbAlign
                    )
                  , ("z-index", "6")
                  ]
              ]
              content
          ]
    _ -> []
-- | A toolbar floating at an edge's label position. Rendered in flow
-- coordinates but inverse-scaled so it keeps a constant size — include
-- it via 'fvcViewportChildren' (it must live inside the viewport):
--
-- @
-- fvcViewportChildren = \scene ->
--   [ v
--   | e <- sceneEdges scene, edgeSelected e
--   , v <- edgeToolbarView cfg scene (edgeId e) [ … ]
--   ]
-- @
edgeToolbarView
  :: FlowViewConfig ctx n e model action
  -> FlowScene n e
  -> EdgeId
  -> [View ctx model action]
  -- ^ toolbar content
  -> [View ctx model action]
edgeToolbarView FlowViewConfig {..} FlowScene {..} eid content =
  case [ e | e <- sceneEdges, edgeId e == eid ] of
    (e : _)
      | Just ep <- edgePathFor fvcConnectionMode sceneNodeLookup e ->
          [ H.div_
              [ key_ ("edge-toolbar-" <> eid)
              , P.class_ "miso-flow__edge-toolbar"
              , style_
                  [ ( "transform"
                    , getEdgeToolbarTransform
                        (edgePathLabelX ep) (edgePathLabelY ep)
                        (viewportZoom sceneViewport)
                        AlignXCenter AlignYCenter
                    )
                  ]
              ]
              content
          ]
    _ -> []
-----------------------------------------------------------------------------
-- | Resolve an edge's endpoints against the lookup; 'Nothing' until
-- both endpoint nodes are measured.
edgePositionFor :: ConnectionMode -> NodeLookup n -> Edge e -> Maybe EdgePosition
edgePositionFor connectionMode nodeLookup e = do
  sourceNode <- M.lookup (edgeSource e) nodeLookup
  targetNode <- M.lookup (edgeTarget e) nodeLookup
  getEdgePosition
    sourceNode (edgeSourceHandle e)
    targetNode (edgeTargetHandle e)
    connectionMode
-----------------------------------------------------------------------------
-- | Resolve an edge's endpoints against the lookup and produce its
-- path, honoring the edge's @type@ (@default@ \/ bezier, @straight@,
-- @step@, @smoothstep@, @simplebezier@). 'Nothing' until both endpoint
-- nodes are measured.
edgePathFor :: ConnectionMode -> NodeLookup n -> Edge e -> Maybe EdgePath
edgePathFor connectionMode nodeLookup e =
  pathByType (fromMaybe "default" (edgeType e))
    <$> edgePositionFor connectionMode nodeLookup e
-----------------------------------------------------------------------------
pathByType :: MisoString -> EdgePosition -> EdgePath
pathByType ty EdgePosition {..} = case ty of
  "straight" -> getStraightPath epSourceX epSourceY epTargetX epTargetY
  "step" -> smoothStep 0
  "smoothstep" -> smoothStep 5
  "simplebezier" ->
    getSimpleBezierPath
      epSourceX epSourceY epSourcePosition
      epTargetX epTargetY epTargetPosition
  _ ->
    getBezierPath
      (bezierPathParams epSourceX epSourceY epTargetX epTargetY)
        { bezierSourcePosition = epSourcePosition
        , bezierTargetPosition = epTargetPosition
        }
  where
    smoothStep radius =
      getSmoothStepPath
        (smoothStepPathParams epSourceX epSourceY epTargetX epTargetY)
          { smoothSourcePosition = epSourcePosition
          , smoothTargetPosition = epTargetPosition
          , smoothBorderRadius = radius
          }
-----------------------------------------------------------------------------
markerDefs :: MisoString -> [Edge e] -> View ctx model action
markerDefs flowId edges =
  S.defs_ [] (map markerDef (createMarkerIds edges (Just flowId) Nothing Nothing Nothing))
  where
    markerDef (MarkerProps mid EdgeMarker {..}) =
      S.marker_
        [ P.id_ mid
        , key_ mid
        , P.class_ "miso-flow__arrowhead"
        , SP.markerWidth_ (jsShow (fromMaybe 12.5 markerWidth))
        , SP.markerHeight_ (jsShow (fromMaybe 12.5 markerHeight))
        , SP.viewBox_ "-10 -10 20 20"
        , SP.markerUnits_ (fromMaybe "strokeWidth" markerUnits)
        , SP.orient_ (fromMaybe "auto-start-reverse" markerOrient)
        , SP.refX_ "0"
        , SP.refY_ "0"
        ]
        [ arrowSymbol markerType markerColor markerStrokeWidth ]
    arrowSymbol mt color strokeWidth =
      S.polyline_
        ( [ SP.stroke_ (fromMaybe "var(--mf-edge)" color)
          , SP.strokeLinecap_ "round"
          , SP.strokeLinejoin_ "round"
          , SP.strokeWidth_ (jsShow (fromMaybe 1 strokeWidth))
          ]
       <> case mt of
            MarkerArrow ->
              [ SP.points_ "-5,-4 0,0 -5,4", SP.fill_ "none" ]
            MarkerArrowClosed ->
              [ SP.points_ "-5,-4 0,0 -5,4 -5,-4"
              , SP.fill_ (fromMaybe "var(--mf-edge)" color)
              ]
        )
-----------------------------------------------------------------------------
-- | Shift\/meta\/ctrl pressed during the event?
multiModifierDecoder :: Decoder Bool
multiModifierDecoder = Decoder
  { decodeAt = DecodeTarget mempty
  , decoder = withObject "event" $ \o ->
      (\sk mk ck -> sk || mk || ck)
        <$> o .: "shiftKey"
        <*> o .: "metaKey"
        <*> o .: "ctrlKey"
  }
-----------------------------------------------------------------------------
selectionRectView :: Maybe Rect -> [View ctx model action]
selectionRectView Nothing = []
selectionRectView (Just (Rect x y w h)) =
  [ H.div_
      [ P.class_ "miso-flow__selection"
      , style_
          [ ("left", jsShow x <> "px")
          , ("top", jsShow y <> "px")
          , ("width", jsShow w <> "px")
          , ("height", jsShow h <> "px")
          ]
      ]
      []
  ]
-----------------------------------------------------------------------------
-- * Connection line
-----------------------------------------------------------------------------
connectionLineView
  :: FlowViewConfig ctx n e model action
  -> ConnectionState n
  -> [View ctx model action]
connectionLineView _ NoConnection = []
connectionLineView FlowViewConfig {..} (InProgress cip) =
  [ S.svg_
      [ P.class_ "miso-flow__connectionline" ]
      [ S.path_
          [ P.classes_ $ catMaybes
              [ Just "miso-flow__connection-path"
              , (\v -> if v then "valid" else "invalid") <$> cipIsValid cip
              ]
          , SP.d_ (edgePath (connectionPathFor fvcConnectionLineType cip))
          ]
      ]
  ]
-----------------------------------------------------------------------------
-- | Path of the in-progress connection line, from the source handle to
-- the pointer.
connectionPathFor :: ConnectionLineType -> ConnectionInProgress n -> EdgePath
connectionPathFor lineType ConnectionInProgress {..} =
  pathByType ty
    EdgePosition
      { epSourceX = xyX cipFrom
      , epSourceY = xyY cipFrom
      , epTargetX = xyX cipTo
      , epTargetY = xyY cipTo
      , epSourcePosition = cipFromPosition
      , epTargetPosition = cipToPosition
      }
  where
    ty = case lineType of
      ConnectionLineBezier -> "default"
      ConnectionLineStraight -> "straight"
      ConnectionLineStep -> "step"
      ConnectionLineSmoothStep -> "smoothstep"
      ConnectionLineSimpleBezier -> "simplebezier"
-----------------------------------------------------------------------------
-- * Background
-----------------------------------------------------------------------------
backgroundView :: MisoString -> Viewport -> View ctx model action
backgroundView flowId Viewport {..} =
  S.svg_
    [ P.class_ "miso-flow__background" ]
    [ S.pattern_
        [ P.id_ patternId
        , SP.x_ (jsShow (viewportX `fmod` gap))
        , SP.y_ (jsShow (viewportY `fmod` gap))
        , P.width_ (jsShow gap)
        , P.height_ (jsShow gap)
        , SP.patternUnits_ "userSpaceOnUse"
        ]
        [ S.circle_
            [ SP.cx_ (jsShow radius)
            , SP.cy_ (jsShow radius)
            , SP.r_ (jsShow radius)
            , SP.fill_ "var(--mf-dots)"
            ]
        ]
    , S.rect_
        [ SP.x_ "0", SP.y_ "0"
        , P.width_ "100%", P.height_ "100%"
        , SP.fill_ ("url(#" <> patternId <> ")")
        ]
    ]
  where
    patternId = "miso-flow__bg-" <> flowId
    gap = 20 * viewportZoom
    radius = viewportZoom
    fmod a b = a - b * fromIntegral (floor (a / b) :: Integer)
-----------------------------------------------------------------------------
-- * Panels & controls
-----------------------------------------------------------------------------
-- | A positioned overlay panel (see @.miso-flow__panel@ in
-- "Miso.Flow.Style").
panelView
  :: PanelPosition
  -> [View ctx model action]
  -> View ctx model action
panelView position =
  H.div_ [ P.classes_ ("miso-flow__panel" : positionClasses position) ]
  where
    positionClasses = \case
      TopLeft      -> [ "top", "left" ]
      TopCenter    -> [ "top", "center-h" ]
      TopRight     -> [ "top", "right" ]
      BottomLeft   -> [ "bottom", "left" ]
      BottomCenter -> [ "bottom", "center-h" ]
      BottomRight  -> [ "bottom", "right" ]
      CenterLeft   -> [ "left", "center-v" ]
      CenterRight  -> [ "right", "center-v" ]
-----------------------------------------------------------------------------
-- | A column of control buttons, one per @(label, action)@.
controlsView
  :: PanelPosition
  -> [(MisoString, action)]
  -> View ctx model action
controlsView position buttons =
  panelView position
    [ H.div_
        [ P.class_ "miso-flow__controls" ]
        [ H.button_
            [ P.class_ "miso-flow__controls-button"
            , P.type_ "button"
            , H.onClick action
            ]
            [ text label ]
        | (label, action) <- buttons
        ]
    ]
-----------------------------------------------------------------------------
-- * Minimap
-----------------------------------------------------------------------------
data MinimapConfig = MinimapConfig
  { mmWidth :: !Double
  , mmHeight :: !Double
  , mmPosition :: !PanelPosition
  , mmPannable :: !Bool
  , mmZoomable :: !Bool
  , mmInversePan :: !Bool
  , mmZoomStep :: !Double
  , mmNodeBorderRadius :: !Double
  , mmOffsetScale :: !Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | xyflow's minimap defaults (pannable\/zoomable on, bottom right).
defaultMinimapConfig :: MinimapConfig
defaultMinimapConfig = MinimapConfig
  { mmWidth = 200
  , mmHeight = 150
  , mmPosition = BottomRight
  , mmPannable = True
  , mmZoomable = True
  , mmInversePan = False
  , mmZoomStep = 1
  , mmNodeBorderRadius = 5
  , mmOffsetScale = 5
  }
-----------------------------------------------------------------------------
-- | viewBB, boundingRect and viewScale; the math of the framework
-- packages' MiniMap component.
minimapGeometry
  :: MinimapConfig
  -> Dimensions          -- ^ container size
  -> Viewport
  -> NodeLookup n
  -> (Rect, Rect, Double)
minimapGeometry MinimapConfig {..} (Dimensions w h) (Viewport tx ty tz) nodeLookup =
  (viewBB, boundingRect, viewScale)
  where
    viewBB = Rect (negate tx / tz) (negate ty / tz) (w / tz) (h / tz)
    visible = not . nodeHidden . internalUserNode
    boundingRect
      | any visible (M.elems nodeLookup) =
          getBoundsOfRects (getInternalNodesBounds visible nodeLookup) viewBB
      | otherwise = viewBB
    viewScale =
      max (rectWidth boundingRect / mmWidth) (rectHeight boundingRect / mmHeight)
-----------------------------------------------------------------------------
-- | The scale the XYMinimap gesture code needs (the component layer
-- forwards it over the bridge whenever the scene changes).
minimapViewScaleFor
  :: MinimapConfig -> Dimensions -> Viewport -> NodeLookup n -> Double
minimapViewScaleFor mm dims viewport nodeLookup =
  let (_, _, viewScale) = minimapGeometry mm dims viewport nodeLookup in viewScale
-----------------------------------------------------------------------------
-- | A pannable\/zoomable overview map. Include it in 'fvcChildren'.
minimapView
  :: MinimapConfig
  -> FlowViewConfig ctx n e model action
  -> FlowScene n e
  -> View ctx model action
minimapView mm@MinimapConfig {..} FlowViewConfig {..} FlowScene {..} =
  panelView mmPosition
    [ H.div_
        [ P.class_ "miso-flow__minimap" ]
        [ S.svg_
            [ P.width_ (jsShow mmWidth)
            , P.height_ (jsShow mmHeight)
            , SP.viewBox_ (jsShow x <> " " <> jsShow y <> " " <> jsShow width <> " " <> jsShow height)
            , onCreatedWith (hookMinimapCreated fvcHooks mm)
            ]
            ( [ S.rect_
                  [ key_ (nodeId u)
                  , P.classes_ $ catMaybes
                      [ Just "miso-flow__minimap-node"
                      , justWhen (nodeSelected u) "selected"
                      ]
                  , SP.x_ (jsShow (xyX (internalPositionAbsolute n)))
                  , SP.y_ (jsShow (xyY (internalPositionAbsolute n)))
                  , P.width_ (jsShow nw)
                  , P.height_ (jsShow nh)
                  , SP.rx_ (jsShow mmNodeBorderRadius)
                  ]
              | n <- M.elems sceneNodeLookup
              , let u = internalUserNode n
              , let Dimensions nw nh = getInternalNodeDimensions n
              , not (nodeHidden u)
              , nw > 0 && nh > 0
              ]
           <> [ S.path_
                  [ P.class_ "miso-flow__minimap-mask"
                  , SP.d_ maskPath
                  ]
              ]
            )
        ]
    ]
  where
    (viewBB, boundingRect, viewScale) =
      minimapGeometry mm sceneDimensions sceneViewport sceneNodeLookup
    viewWidth = viewScale * mmWidth
    viewHeight = viewScale * mmHeight
    offset = mmOffsetScale * viewScale
    x = rectX boundingRect - (viewWidth - rectWidth boundingRect) / 2 - offset
    y = rectY boundingRect - (viewHeight - rectHeight boundingRect) / 2 - offset
    width = viewWidth + offset * 2
    height = viewHeight + offset * 2
    maskPath = mconcat
      [ "M", jsShow (x - offset), ",", jsShow (y - offset)
      , "h", jsShow (width + offset * 2)
      , "v", jsShow (height + offset * 2)
      , "h", jsShow (negate (width + offset * 2))
      , "z M", jsShow (rectX viewBB), ",", jsShow (rectY viewBB)
      , "h", jsShow (rectWidth viewBB)
      , "v", jsShow (rectHeight viewBB)
      , "h", jsShow (negate (rectWidth viewBB))
      , "z"
      ]
-----------------------------------------------------------------------------
-- * Node resizer
-----------------------------------------------------------------------------
data ResizerConfig = ResizerConfig
  { rcMinWidth :: !Double
  , rcMinHeight :: !Double
  , rcMaxWidth :: !(Maybe Double)
  , rcMaxHeight :: !(Maybe Double)
  , rcKeepAspectRatio :: !Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
defaultResizerConfig :: ResizerConfig
defaultResizerConfig = ResizerConfig
  { rcMinWidth = 10
  , rcMinHeight = 10
  , rcMaxWidth = Nothing
  , rcMaxHeight = Nothing
  , rcKeepAspectRatio = False
  }
-----------------------------------------------------------------------------
-- | Resize controls for one node: four edge lines plus four corner
-- handles, wired to XYResizer through the bridge. Render them inside the
-- node's content (typically only while the node is selected):
--
-- @
-- myContent cfg n =
--   nodeResizerView defaultResizerConfig cfg n <> defaultNodeContent label cfg n
-- @
nodeResizerView
  :: ResizerConfig
  -> FlowViewConfig ctx n e model action
  -> Node n
  -> [View ctx model action]
nodeResizerView ResizerConfig {..} FlowViewConfig {..} n =
  map (control "line") (map ControlLine [ ControlLineTop, ControlLineBottom, ControlLineLeft, ControlLineRight ])
    <> map (control "handle") xyResizerHandlePositions
  where
    control variant position =
      H.div_
        [ P.classes_
            [ "miso-flow__resize-control"
            , "nodrag"
            , variant
            , controlPositionToText position
            ]
        , textProp "data-resizer" (controlPositionToText position)
        , onCreatedWith
            (hookResizerCreated fvcHooks (nodeId n) (resizeParams position))
        , onBeforeDestroyedWith (hookResizerBeforeDestroyed fvcHooks (nodeId n))
        ]
        []
    resizeParams position = object $
      [ "controlPosition" .= controlPositionToText position
      , "minWidth" .= rcMinWidth
      , "minHeight" .= rcMinHeight
      , "keepAspectRatio" .= rcKeepAspectRatio
      ]
      <> catMaybes
      [ ("maxWidth" .=) <$> rcMaxWidth
      , ("maxHeight" .=) <$> rcMaxHeight
      ]
-----------------------------------------------------------------------------
