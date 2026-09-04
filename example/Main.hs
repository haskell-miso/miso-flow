-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
-----------------------------------------------------------------------------
-- | Patchwork: a modular-synth-style signal patch built with miso-flow.
--
-- Every miso-flow capability in one canvas: drag modules (and whole
-- groups), patch cables between ports (with a connection rule: sources
-- can't feed the output directly), reconnect cables by their anchors,
-- multi-select with modifier keys or a shift-drag box, resize modules,
-- delete via toolbars or the keyboard, snap to grid, and navigate with
-- the minimap (click it to center).
--
-- Build for the browser (see README.md), serve @example/static@, and
-- open it. @js\/miso-flow.js@ must be served next to the compiled app.
----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso (App, CSS (Style), defaultEvents, startApp)
import           Miso.CSS (style_)
import           Miso.Html (button_, div_, onClick, span_)
import qualified Miso.Html.Property as P
import           Miso.String (MisoString, ms)
import           Miso.Types (Component (styles), View, text)
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso.Flow
import           Miso.Flow.Utils (internalNodeToRect, pointToRendererPoint)
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | A module's name and a one-line description.
type Module = (MisoString, MisoString)
-----------------------------------------------------------------------------
app :: App (FlowModel Module ()) (FlowAction Module ())
app =
  (flowComponent options settings (text . fst) customize initialNodes initialEdges)
    { styles = [ flowCSS, Style patchworkStyles ] }
  where
    options = defaultStoreOptions
      { soMinZoom = 0.2
      , soMaxZoom = 4
      , soSnapGrid = SnapGrid 20 20
        -- panning is clamped to a generous region around the patch
      , soTranslateExtent = Just (coordinateExtent (-2400, -1800) (3600, 2400))
      }
    settings = defaultFlowSettings
      { fsValidateConnection = Just signalRule }
    customize cfg = cfg
      { fvcDark = True
      , fvcEdgesReconnectable = True
      , fvcAttrs =
          [ style_ [ ("width", "100vw"), ("height", "100vh") ] ]
      , fvcNodeContent = moduleContent
      , fvcChildren = overlays cfg
      , fvcViewportChildren = cableToolbars cfg
      }
-----------------------------------------------------------------------------
-- | The patch rule: oscillators and LFOs must pass through at least one
-- processor, so a source can never feed the main out directly.
signalRule :: Connection -> Bool
signalRule c =
  not (connectionSource c `elem` sources && connectionTarget c == "out")
  where
    sources = [ "osc-a", "lfo" ] :: [NodeId]
-----------------------------------------------------------------------------
-- * Graph
-----------------------------------------------------------------------------
initialNodes :: [Node Module]
initialNodes =
  [ (node "osc-a" (xy 0 60) ("Oscillator A", "saw wave, 220 Hz"))
      { nodeType = Just "source", nodeSourcePosition = Just PositionRight }
  , (node "lfo" (xy 0 300) ("LFO", "sine, 2 Hz"))
      { nodeType = Just "source", nodeSourcePosition = Just PositionRight }
    -- the voice group: a subflow whose children move with it
  , (node "voice" (xy 280 (-10)) ("Voice", ""))
      { nodeType = Just "group"
      , nodeWidth = Just 420
      , nodeHeight = Just 230
      , nodeConnectable = Just False
      }
  , (node "env" (xy 30 60) ("Envelope", "attack 12 ms"))
      { nodeType = Just "effect"
      , nodeParentId = Just "voice"
      , nodeExtent = Just ExtentParent
      }
  , (node "filt" (xy 230 90) ("Filter", "low pass, 800 Hz"))
      { nodeType = Just "filter"
      , nodeParentId = Just "voice"
      , nodeExtent = Just ExtentParent
      }
    -- draggable only by its grip, via a dragHandle selector
  , (node "delay" (xy 790 90) ("Delay", "dotted eighth"))
      { nodeType = Just "effect", nodeDragHandle = Just ".pw-grip" }
    -- the main out can be hidden but never deleted
  , (node "out" (xy 1060 210) ("Main out", "stereo"))
      { nodeType = Just "output"
      , nodeTargetPosition = Just PositionLeft
      , nodeDeletable = Just False
      }
  ]
-----------------------------------------------------------------------------
initialEdges :: [Edge ()]
initialEdges =
  [ (edge "c-osc-env" "osc-a" "env") { edgeAnimated = True }
  , (edge "c-env-filt" "env" "filt")
      { edgeTargetHandle = Just "in", edgeType = Just "straight" }
  , (edge "c-lfo-cv" "lfo" "filt")
      { edgeTargetHandle = Just "cv", edgeType = Just "smoothstep" }
  , (edge "c-filt-delay" "filt" "delay")
      { edgeType = Just "simplebezier"
      , edgeMarkerStart = Just (Marker (edgeMarker MarkerArrow)
          { markerColor = Just "#8e9bff", markerStrokeWidth = Just 1.5 })
      , edgeMarkerEnd = Just (Marker (edgeMarker MarkerArrowClosed))
      }
  , (edge "c-delay-out" "delay" "out")
      { edgeAnimated = True
      , edgeMarkerEnd = Just (Marker (edgeMarker MarkerArrowClosed))
      }
  ]
-----------------------------------------------------------------------------
-- * Modules
-----------------------------------------------------------------------------
type PatchView = View () (FlowModel Module ()) (FlowAction Module ())
type PatchConfig =
  FlowViewConfig () Module () (FlowModel Module ()) (FlowAction Module ())
-----------------------------------------------------------------------------
moduleContent :: PatchConfig -> Node Module -> [PatchView]
moduleContent cfg n = case nodeType n of
  Just "group" ->
    resizer
      <> [ div_ [ P.class_ "pw-group-title" ] [ text (fst (nodeData n)) ] ]
  Just "source" ->
    resizer <> card <> [ handleView cfg n SourceHandle PositionRight Nothing ]
  Just "output" ->
    resizer <> card <> [ handleView cfg n TargetHandle PositionLeft Nothing ]
  Just "filter" ->
    resizer <> card
      <> [ handleView cfg n TargetHandle PositionLeft (Just "in")
         , handleView cfg n TargetHandle PositionLeft (Just "cv")
         , handleView cfg n SourceHandle PositionRight Nothing
         ]
  _ ->
    resizer <> card
      <> [ handleView cfg n TargetHandle PositionLeft Nothing
         , handleView cfg n SourceHandle PositionRight Nothing
         ]
  where
    (name, desc) = nodeData n
    resizer =
      [ v | nodeSelected n, v <- nodeResizerView defaultResizerConfig cfg n ]
    card =
      [ div_
          [ P.class_ "pw-module" ]
          ( [ span_ [ P.class_ "pw-grip" ] [ "\x2059" ]
            | nodeDragHandle n == Just ".pw-grip"
            ]
         <> [ div_ [ P.class_ "pw-name" ] [ text name ]
            , div_ [ P.class_ "pw-desc" ] [ text desc ]
            ]
          )
      ]
-----------------------------------------------------------------------------
-- * Chrome
-----------------------------------------------------------------------------
overlays :: PatchConfig -> FlowScene Module () -> [PatchView]
overlays cfg scene =
  [ panelView TopLeft
      [ div_
          [ P.class_ "pw-panel pw-brand" ]
          [ div_ [ P.class_ "pw-title" ] [ "Patchwork" ]
          , div_
              [ P.class_ "pw-hint" ]
              [ "Drag modules, patch cables between ports. Sources need "
              , "a processor before the main out. Add modules from the "
              , "palette, shift-drag to select, delete to remove, click "
              , "the minimap to recenter."
              ]
          ]
      ]
  , panelView TopRight
      [ div_
          [ P.class_ "pw-stack" ]
          ( [ div_
                [ P.class_ "pw-panel pw-status" ]
                [ text (ms (length (sceneNodes scene)) <> " modules, "
                    <> ms (length (sceneEdges scene)) <> " cables, zoom "
                    <> ms (round (viewportZoom (sceneViewport scene) * 100) :: Int)
                    <> "%")
                ]
            ]
         <> [ button_
                [ P.class_ "pw-panel pw-toggle"
                , P.type_ "button"
                , onClick (FlowSetNodes
                    [ n { nodeHidden = False } | n <- sceneNodes scene ])
                ]
                [ text ("Show " <> ms hiddenCount <> " hidden") ]
            | hiddenCount > 0
            ]
         <> [ button_
                [ P.classes_ ("pw-panel" : "pw-toggle" : [ "on" | snapping ])
                , P.type_ "button"
                , onClick (FlowOptionsChanged opts { soSnapToGrid = not snapping })
                ]
                [ span_ [ P.class_ "pw-toggle-dot" ] []
                , text (if snapping then "Snap to grid: on" else "Snap to grid: off")
                ]
            ]
          )
      ]
  , panelView CenterLeft
      [ div_
          [ P.class_ "pw-panel pw-palette" ]
          [ button_ [ P.type_ "button", onClick (addModule "source") ] [ "Add oscillator" ]
          , button_ [ P.type_ "button", onClick (addModule "effect") ] [ "Add effect" ]
          , button_ [ P.type_ "button", onClick (addModule "output") ] [ "Add output" ]
          ]
      ]
  , controlsView BottomLeft
      [ ("+", FlowZoomIn)
      , ("\x2212", FlowZoomOut)
      , ("1:1", FlowZoomTo 1)
      , ("\x2922", FlowFitView)
      ]
  , minimapView defaultMinimapConfig cfg scene
  ]
  <> [ v
     | n <- sceneNodes scene
     , nodeSelected n
     , let buttons = toolbarButtons scene n
     , not (null buttons)
     , v <- nodeToolbarView defaultToolbarConfig scene (nodeId n) buttons
     ]
  where
    opts = sceneOptions scene
    snapping = soSnapToGrid opts
    hiddenCount = length [ () | n <- sceneNodes scene, nodeHidden n ]
    -- spawn a fresh module at the viewport center; the graph is plain
    -- model data, so adding is just FlowSetNodes with one more node
    addModule kind =
      FlowSetNodes (sceneNodes scene <> [ newModule scene kind ])
-----------------------------------------------------------------------------
-- | Per-module toolbar actions: hide (except groups), frame the
-- viewport on groups, delete unless the module forbids it.
toolbarButtons :: FlowScene Module () -> Node Module -> [PatchView]
toolbarButtons scene n =
  [ button_
      [ onClick (FlowFitBounds rect) ]
      [ "frame" ]
  | nodeType n == Just "group"
  , Just rect <- [ internalNodeToRect
                     <$> lookupNode (nodeId n) (sceneNodeLookup scene) ]
  ]
  <>
  [ button_
      [ onClick (FlowSetNodes
          [ if nodeId n' == nodeId n
              then n' { nodeHidden = True, nodeSelected = False }
              else n'
          | n' <- sceneNodes scene
          ]) ]
      [ "hide" ]
  | nodeType n /= Just "group"
  ]
  <>
  [ button_
      [ onClick (FlowRemoveNode (nodeId n)) ]
      [ "delete" ]
  | nodeDeletable n /= Just False
  ]
  where
    lookupNode = M.lookup
-----------------------------------------------------------------------------
-- | A fresh module of the given kind, placed at the viewport center
-- (nudged by how many modules exist, so repeated adds cascade).
newModule :: FlowScene Module () -> MisoString -> Node Module
newModule scene kind =
  base { nodeType = Just kind
       , nodeSourcePosition = Just PositionRight
       , nodeTargetPosition = Just PositionLeft
       }
  where
    existing = map nodeId (sceneNodes scene)
    fresh = head
      [ candidate
      | i <- [ 1 :: Int .. ]
      , let candidate = kind <> "-" <> ms i
      , candidate `notElem` existing
      ]
    n = length (sceneNodes scene)
    Dimensions w h = sceneDimensions scene
    centre = pointToRendererPoint
      (xy (w / 2) (h / 2)) (sceneViewport scene) False (SnapGrid 1 1)
    position = xy
      (xyX centre + fromIntegral (n `mod` 5) * 28 - 90)
      (xyY centre + fromIntegral (n `mod` 5) * 22 - 60)
    base = case kind of
      "source" -> node fresh position ("Oscillator " <> ms (length existing), "sine wave")
      "output" -> node fresh position ("Out " <> ms (length existing), "stereo")
      _ -> node fresh position ("Effect " <> ms (length existing), "empty slot")
-----------------------------------------------------------------------------
-- | A small toolbar on each selected cable to unpatch it.
cableToolbars :: PatchConfig -> FlowScene Module () -> [PatchView]
cableToolbars cfg scene =
  -- a permanent tag on the control cable (edge labels are just views
  -- at the cable's label anchor)
  [ v
  | v <- edgeToolbarView cfg scene "c-lfo-cv"
      [ span_ [ P.class_ "pw-cable-tag" ] [ "control" ] ]
  ]
  <>
  [ v
  | e <- sceneEdges scene
  , edgeSelected e
  , v <- edgeToolbarView cfg scene (edgeId e)
      [ button_
          [ onClick (FlowSetEdges
              [ e' | e' <- sceneEdges scene, edgeId e' /= edgeId e ]) ]
          [ "unpatch" ]
      ]
  ]
-----------------------------------------------------------------------------
-- * Patchwork styles (on top of 'flowCSS')
-----------------------------------------------------------------------------
patchworkStyles :: MisoString
patchworkStyles = mconcat
  [ "html, body { margin: 0; height: 100%; overflow: hidden; background: #151830; }",
    ".pw-stack { display: flex; flex-direction: column; align-items: flex-end; gap: 8px; }",
    ".pw-status { font-size: 11.5px; padding: 7px 12px; color: #9aa3d5; }",
    ".pw-palette { display: flex; flex-direction: column; gap: 2px; padding: 6px; }",
    ".pw-palette button { border: none; background: transparent; color: var(--mf-panel-color); font-size: 12px; text-align: left; padding: 7px 11px; border-radius: 8px; cursor: pointer; }",
    ".pw-palette button:hover { background: var(--mf-accent-soft); }",
    ".pw-palette button:focus-visible { outline: 2px solid var(--mf-accent); }",
    ".pw-grip { position: absolute; top: 7px; right: 9px; font-size: 11px; color: #9aa3d5; cursor: grab; }",
    ".pw-cable-tag { font-size: 10px; color: #9aa3d5; background: var(--mf-edge-label-bg); border: 1px solid var(--mf-panel-border); padding: 2px 8px; border-radius: 6px; }",
  ".pw-module {  background: var(--mf-node-bg);  border: 1px solid var(--mf-node-border);  border-left: 3px solid var(--mf-accent);  border-radius: 10px;  box-shadow: var(--mf-node-shadow);  padding: 10px 16px 11px 13px;  min-width: 150px;  width: 100%;  height: 100%;  box-sizing: border-box;  transition: box-shadow 120ms ease;}",
  ".miso-flow__node-source .pw-module { border-left-color: #ffad4d; }",
  ".miso-flow__node-effect .pw-module,.miso-flow__node-filter .pw-module { border-left-color: #8e9bff; }",
  ".miso-flow__node-output .pw-module { border-left-color: #4ade80; }",
  ".miso-flow__node.selected .pw-module { box-shadow: var(--mf-node-shadow-selected); }",
  ".pw-name {  font-size: 13px;  font-weight: 600;  letter-spacing: 0.01em;  color: var(--mf-node-color);}",
  ".pw-desc { font-size: 11px; color: #9aa3d5; margin-top: 3px; }",
  ".miso-flow__node-group {  border: 1.5px dashed #3a4170;  border-radius: 14px;  background: rgba(142, 155, 255, 0.05);}",
  ".miso-flow__node-group.selected { border-color: var(--mf-accent); }",
  ".pw-group-title {  position: absolute;  top: 9px;  left: 13px;  font-size: 11px;  font-weight: 600;  letter-spacing: 0.04em;  color: #9aa3d5;}",
  ".miso-flow__handle[data-handleid=\"in\"] { top: 34%; }",
  ".miso-flow__handle[data-handleid=\"cv\"] {  top: 72%;  border-color: #8e9bff;}",
  ".miso-flow__edge-smoothstep .miso-flow__edge-path { stroke: #8e9bff; opacity: 0.75; }",
  ".pw-panel {  background: var(--mf-panel-bg);  border: 1px solid var(--mf-panel-border);  border-radius: 12px;  padding: 12px 16px;  backdrop-filter: blur(10px);  color: var(--mf-panel-color);  font-family: var(--mf-font);}",
  ".pw-title {  font-size: 15px;  font-weight: 700;  letter-spacing: -0.01em;  color: #e9ebfa;}",
  ".pw-hint {  margin-top: 5px;  font-size: 11.5px;  line-height: 1.55;  max-width: 250px;  color: #9aa3d5;}",
  ".pw-toggle {  display: flex;  align-items: center;  gap: 8px;  font-size: 12px;  cursor: pointer;  padding: 8px 14px;}",
  ".pw-toggle-dot {  width: 8px;  height: 8px;  border-radius: 50%;  background: #3a4170;  transition: background 120ms ease;}",
  ".pw-toggle.on .pw-toggle-dot { background: var(--mf-accent); }",
  ".pw-toggle:focus-visible { outline: 2px solid var(--mf-accent); outline-offset: 2px; }",
  ".miso-flow__node-toolbar button:focus-visible,.miso-flow__controls-button:focus-visible { outline: 2px solid var(--mf-accent); }"
  ]
-----------------------------------------------------------------------------
