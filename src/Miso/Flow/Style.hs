-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Style
-- License     :  BSD3-style (see the file LICENSE)
--
-- Stylesheets for miso-flow, split the way xyflow splits @base.css@ and
-- @style.css@:
--
-- * 'flowBaseStyles' — the structural contract the gesture system
--   depends on (absolutely positioned pane \/ viewport \/ nodes, the
--   transformed viewport, pointer-events routing, handle and resizer
--   geometry). Every app needs these.
-- * 'flowThemeStyles' — the default look: color tokens (light, plus a
--   dark palette behind a @dark@ class on the container), node cards,
--   cables, panels, controls, minimap, toolbars.
--
-- 'flowStyles' is both together; 'flowCSS' wraps it for a component's
-- @styles@ field. To ship your own look, use 'flowBaseCSS' and provide
-- your own rules instead of the theme — the views reference the
-- @--mf-*@ custom properties (see 'flowThemeStyles' for the full list),
-- so either define those tokens or restyle the classes directly.
----------------------------------------------------------------------------
module Miso.Flow.Style
  ( -- * Everything
    flowStyles
  , flowCSS
    -- * Structural contract only
  , flowBaseStyles
  , flowBaseCSS
    -- * Default theme only
  , flowThemeStyles
  , flowThemeCSS
  ) where
-----------------------------------------------------------------------------
import           Miso (CSS (Style))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Base + default theme, ready for a component's @styles@ field.
flowCSS :: CSS
flowCSS = Style flowStyles
-----------------------------------------------------------------------------
-- | Structural contract only, for apps shipping their own theme.
flowBaseCSS :: CSS
flowBaseCSS = Style flowBaseStyles
-----------------------------------------------------------------------------
-- | Default theme only.
flowThemeCSS :: CSS
flowThemeCSS = Style flowThemeStyles
-----------------------------------------------------------------------------
-- | The full stylesheet: 'flowBaseStyles' plus 'flowThemeStyles'.
flowStyles :: MisoString
flowStyles = flowBaseStyles <> flowThemeStyles
-----------------------------------------------------------------------------
-- | Layout and interaction rules the bridge requires; no colors, no
-- decoration. Do not override positioning, transforms or pointer-events
-- in these classes.
flowBaseStyles :: MisoString
flowBaseStyles = "\
\.miso-flow {\
\  position: relative;\
\  width: 100%;\
\  height: 100%;\
\  overflow: hidden;\
\  z-index: 0;\
\}\
\.miso-flow__renderer,\
\.miso-flow__pane {\
\  position: absolute;\
\  inset: 0;\
\  width: 100%;\
\  height: 100%;\
\}\
\.miso-flow__pane { cursor: grab; z-index: 1; }\
\.miso-flow__pane.dragging { cursor: grabbing; }\
\.miso-flow__viewport {\
\  position: absolute;\
\  inset: 0;\
\  width: 100%;\
\  height: 100%;\
\  transform-origin: 0 0;\
\  pointer-events: none;\
\  z-index: 2;\
\  will-change: transform;\
\}\
\.miso-flow__selection {\
\  position: absolute;\
\  z-index: 4;\
\  pointer-events: none;\
\}\
\.miso-flow__nodes { position: absolute; width: 0; height: 0; }\
\.miso-flow__node {\
\  position: absolute;\
\  pointer-events: all;\
\  transform-origin: 0 0;\
\  box-sizing: border-box;\
\  cursor: default;\
\  user-select: none;\
\  -webkit-user-select: none;\
\  will-change: transform;\
\}\
\.miso-flow__node.nopan { cursor: grab; }\
\.miso-flow__node.dragging { cursor: grabbing; }\
\.miso-flow__edges {\
\  position: absolute;\
\  top: 0;\
\  left: 0;\
\  overflow: visible;\
\  pointer-events: none;\
\}\
\.miso-flow__edge { pointer-events: visibleStroke; }\
\.miso-flow__edge-path { fill: none; }\
\.miso-flow__edge.animated .miso-flow__edge-path {\
\  stroke-dasharray: 5;\
\  animation: miso-flow-dash 0.5s linear infinite;\
\}\
\.miso-flow__edge-interaction {\
\  stroke: transparent;\
\  fill: none;\
\  pointer-events: stroke;\
\  cursor: pointer;\
\}\
\.miso-flow__edge-textwrapper { pointer-events: all; }\
\.miso-flow__edge-anchor {\
\  fill: transparent;\
\  stroke: transparent;\
\  pointer-events: all;\
\  cursor: move;\
\}\
\@keyframes miso-flow-dash { to { stroke-dashoffset: -10; } }\
\@media (prefers-reduced-motion: reduce) {\
\  .miso-flow__edge.animated .miso-flow__edge-path { animation: none; }\
\}\
\.miso-flow__connectionline {\
\  position: absolute;\
\  top: 0;\
\  left: 0;\
\  overflow: visible;\
\  pointer-events: none;\
\  z-index: 1001;\
\}\
\.miso-flow__connection-path { fill: none; }\
\.miso-flow__handle {\
\  position: absolute;\
\  width: 9px;\
\  height: 9px;\
\  pointer-events: none;\
\  min-width: 5px;\
\  min-height: 5px;\
\  box-sizing: border-box;\
\}\
\.miso-flow__handle.connectable {\
\  pointer-events: all;\
\  cursor: crosshair;\
\}\
\.miso-flow__handle-top { top: -5px; left: 50%; transform: translate(-50%, 0); }\
\.miso-flow__handle-bottom { bottom: -5px; left: 50%; transform: translate(-50%, 0); }\
\.miso-flow__handle-left { left: -5px; top: 50%; transform: translate(0, -50%); }\
\.miso-flow__handle-right { right: -5px; top: 50%; transform: translate(0, -50%); }\
\.miso-flow__background {\
\  position: absolute;\
\  inset: 0;\
\  width: 100%;\
\  height: 100%;\
\  pointer-events: none;\
\  z-index: 0;\
\}\
\.miso-flow__panel {\
\  position: absolute;\
\  z-index: 5;\
\  margin: 14px;\
\  pointer-events: all;\
\}\
\.miso-flow__panel.top { top: 0; }\
\.miso-flow__panel.bottom { bottom: 0; }\
\.miso-flow__panel.left { left: 0; }\
\.miso-flow__panel.right { right: 0; }\
\.miso-flow__panel.center-h { left: 50%; transform: translateX(-50%); }\
\.miso-flow__panel.center-v { top: 50%; transform: translateY(-50%); }\
\.miso-flow__minimap-mask { fill-rule: evenodd; pointer-events: none; }\
\.miso-flow__minimap svg { display: block; }\
\.miso-flow__node-toolbar { position: absolute; }\
\.miso-flow__edge-toolbar { position: absolute; pointer-events: all; }\
\.miso-flow__resize-control { position: absolute; pointer-events: all; }\
\.miso-flow__resize-control.handle { width: 8px; height: 8px; }\
\.miso-flow__resize-control.handle.top-left { top: -4px; left: -4px; cursor: nwse-resize; }\
\.miso-flow__resize-control.handle.top-right { top: -4px; right: -4px; cursor: nesw-resize; }\
\.miso-flow__resize-control.handle.bottom-left { bottom: -4px; left: -4px; cursor: nesw-resize; }\
\.miso-flow__resize-control.handle.bottom-right { bottom: -4px; right: -4px; cursor: nwse-resize; }\
\.miso-flow__resize-control.line.top { top: -3px; left: 0; width: 100%; height: 6px; cursor: ns-resize; }\
\.miso-flow__resize-control.line.bottom { bottom: -3px; left: 0; width: 100%; height: 6px; cursor: ns-resize; }\
\.miso-flow__resize-control.line.left { left: -3px; top: 0; width: 6px; height: 100%; cursor: ew-resize; }\
\.miso-flow__resize-control.line.right { right: -3px; top: 0; width: 6px; height: 100%; cursor: ew-resize; }\
\"
-----------------------------------------------------------------------------
-- | The default look. Driven by CSS custom properties on @.miso-flow@
-- (restyle by overriding them): @--mf-bg@, @--mf-dots@, @--mf-node-bg@,
-- @--mf-node-color@, @--mf-node-border@, @--mf-node-shadow@,
-- @--mf-node-shadow-selected@, @--mf-accent@, @--mf-accent-soft@,
-- @--mf-edge@, @--mf-edge-selected@, @--mf-edge-label-bg@,
-- @--mf-handle@, @--mf-handle-border@, @--mf-connection@,
-- @--mf-connection-invalid@, @--mf-connection-valid@, @--mf-panel-bg@,
-- @--mf-panel-border@, @--mf-panel-color@, @--mf-minimap-mask@,
-- @--mf-minimap-node@, @--mf-selection@, @--mf-font@. Add a @dark@
-- class on the container for the dark palette.
flowThemeStyles :: MisoString
flowThemeStyles = mconcat
  [ themeTokens
  , themeSurfaces
  , themeNodes
  , themeEdges
  , themeHandles
  , themePanels
  , themeControls
  , themeMinimap
  , themeToolbar
  , themeResizer
  ]
-----------------------------------------------------------------------------
themeTokens :: MisoString
themeTokens = "\
\.miso-flow {\
\  --mf-bg: #f6f7fb;\
\  --mf-dots: #c3c8d4;\
\  --mf-node-bg: #ffffff;\
\  --mf-node-color: #0f172a;\
\  --mf-node-border: #e2e8f0;\
\  --mf-node-shadow: 0 1px 2px rgba(15, 23, 42, 0.06), 0 8px 24px rgba(15, 23, 42, 0.08);\
\  --mf-node-shadow-selected: 0 0 0 2px var(--mf-accent), 0 12px 32px rgba(79, 70, 229, 0.25);\
\  --mf-accent: #6366f1;\
\  --mf-accent-soft: rgba(99, 102, 241, 0.14);\
\  --mf-edge: #9aa4b6;\
\  --mf-edge-selected: var(--mf-accent);\
\  --mf-edge-label-bg: rgba(255, 255, 255, 0.9);\
\  --mf-handle: #ffffff;\
\  --mf-handle-border: var(--mf-accent);\
\  --mf-connection: var(--mf-accent);\
\  --mf-connection-invalid: #f43f5e;\
\  --mf-connection-valid: #10b981;\
\  --mf-panel-bg: rgba(255, 255, 255, 0.86);\
\  --mf-panel-border: rgba(15, 23, 42, 0.08);\
\  --mf-panel-color: #334155;\
\  --mf-minimap-mask: rgba(148, 163, 184, 0.25);\
\  --mf-minimap-node: #cbd5e1;\
\  --mf-selection: rgba(99, 102, 241, 0.08);\
\  --mf-font: ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto, 'Inter', sans-serif;\
\}\
\.miso-flow.dark {\
\  --mf-bg: #151830;\
\  --mf-dots: #262b4a;\
\  --mf-node-bg: #1d2244;\
\  --mf-node-color: #e9ebfa;\
\  --mf-node-border: #303764;\
\  --mf-node-shadow: 0 1px 2px rgba(6, 8, 22, 0.5), 0 10px 28px rgba(6, 8, 22, 0.45);\
\  --mf-node-shadow-selected: 0 0 0 2px var(--mf-accent), 0 12px 32px rgba(255, 173, 77, 0.22);\
\  --mf-accent: #ffad4d;\
\  --mf-accent-soft: rgba(255, 173, 77, 0.16);\
\  --mf-edge: #6e77a8;\
\  --mf-edge-selected: var(--mf-accent);\
\  --mf-edge-label-bg: rgba(21, 24, 48, 0.9);\
\  --mf-handle: #151830;\
\  --mf-connection: var(--mf-accent);\
\  --mf-connection-invalid: #fb6e8a;\
\  --mf-connection-valid: #4ade80;\
\  --mf-panel-bg: rgba(21, 24, 48, 0.78);\
\  --mf-panel-border: rgba(154, 163, 213, 0.16);\
\  --mf-panel-color: #b9bfe3;\
\  --mf-minimap-mask: rgba(15, 17, 36, 0.62);\
\  --mf-minimap-node: #3a4170;\
\  --mf-selection: rgba(255, 173, 77, 0.1);\
\}"
-----------------------------------------------------------------------------
themeSurfaces :: MisoString
themeSurfaces = "\
\.miso-flow {\
\  background-color: var(--mf-bg);\
\  font-family: var(--mf-font);\
\}\
\.miso-flow__selection {\
\  background: var(--mf-selection);\
\  border: 1px solid var(--mf-accent);\
\}"
-----------------------------------------------------------------------------
themeNodes :: MisoString
themeNodes = "\
\.miso-flow__node-default {\
\  background: var(--mf-node-bg);\
\  color: var(--mf-node-color);\
\  border: 1px solid var(--mf-node-border);\
\  border-radius: 12px;\
\  box-shadow: var(--mf-node-shadow);\
\  padding: 10px 16px;\
\  font-size: 13px;\
\  min-width: 120px;\
\  text-align: center;\
\  transition: box-shadow 120ms ease;\
\  width: 100%;\
\  height: 100%;\
\  box-sizing: border-box;\
\  display: flex;\
\  align-items: center;\
\  justify-content: center;\
\}\
\.miso-flow__node.selected > .miso-flow__node-default {\
\  box-shadow: var(--mf-node-shadow-selected);\
\}"
-----------------------------------------------------------------------------
themeEdges :: MisoString
themeEdges = "\
\.miso-flow__edge-path {\
\  stroke: var(--mf-edge);\
\  stroke-width: 1.5;\
\}\
\.miso-flow__edge.selected .miso-flow__edge-path {\
\  stroke: var(--mf-edge-selected);\
\  stroke-width: 2;\
\}\
\.miso-flow__edge.selected .miso-flow__edge-anchor {\
\  fill: var(--mf-accent-soft);\
\  stroke: var(--mf-accent);\
\  stroke-width: 1;\
\}\
\.miso-flow__edge-label {\
\  font-size: 11px;\
\  fill: var(--mf-panel-color);\
\}\
\.miso-flow__edge-labelbg { fill: var(--mf-edge-label-bg); }\
\.miso-flow__connection-path {\
\  stroke: var(--mf-connection);\
\  stroke-width: 1.5;\
\  stroke-dasharray: 4;\
\}\
\.miso-flow__connection-path.invalid { stroke: var(--mf-connection-invalid); }\
\.miso-flow__connection-path.valid { stroke: var(--mf-connection-valid); }"
-----------------------------------------------------------------------------
themeHandles :: MisoString
themeHandles = "\
\.miso-flow__handle {\
\  background: var(--mf-handle);\
\  border: 2px solid var(--mf-handle-border);\
\  border-radius: 100%;\
\}\
\.miso-flow__handle.connectable:hover {\
\  background: var(--mf-handle-border);\
\}"
-----------------------------------------------------------------------------
themePanels :: MisoString
themePanels = "\
\.miso-flow__attribution {\
\  font-size: 10px;\
\  color: var(--mf-panel-color);\
\  background: var(--mf-panel-bg);\
\  padding: 2px 6px;\
\  border-radius: 6px;\
\  margin: 6px;\
\  opacity: 0.7;\
\}\
\.miso-flow__attribution a { color: inherit; text-decoration: none; }"
-----------------------------------------------------------------------------
themeControls :: MisoString
themeControls = "\
\.miso-flow__controls {\
\  display: flex;\
\  flex-direction: column;\
\  gap: 4px;\
\  background: var(--mf-panel-bg);\
\  border: 1px solid var(--mf-panel-border);\
\  border-radius: 12px;\
\  padding: 6px;\
\  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.12);\
\  backdrop-filter: blur(8px);\
\}\
\.miso-flow__controls-button {\
\  display: flex;\
\  align-items: center;\
\  justify-content: center;\
\  width: 28px;\
\  height: 28px;\
\  border: none;\
\  border-radius: 8px;\
\  background: transparent;\
\  color: var(--mf-panel-color);\
\  font-size: 15px;\
\  cursor: pointer;\
\}\
\.miso-flow__controls-button:hover { background: var(--mf-accent-soft); }\
\.miso-flow__controls-button svg { width: 14px; height: 14px; fill: currentColor; }"
-----------------------------------------------------------------------------
themeMinimap :: MisoString
themeMinimap = "\
\.miso-flow__minimap {\
\  background: var(--mf-panel-bg);\
\  border: 1px solid var(--mf-panel-border);\
\  border-radius: 12px;\
\  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.12);\
\  overflow: hidden;\
\}\
\.miso-flow__minimap-mask { fill: var(--mf-minimap-mask); }\
\.miso-flow__minimap-node { fill: var(--mf-minimap-node); stroke: none; }\
\.miso-flow__minimap-node.selected { fill: var(--mf-accent); }"
-----------------------------------------------------------------------------
themeToolbar :: MisoString
themeToolbar = "\
\.miso-flow__node-toolbar,\
\.miso-flow__edge-toolbar {\
\  display: flex;\
\  gap: 4px;\
\  background: var(--mf-panel-bg);\
\  border: 1px solid var(--mf-panel-border);\
\  border-radius: 10px;\
\  padding: 4px;\
\  box-shadow: 0 8px 24px rgba(15, 23, 42, 0.15);\
\  backdrop-filter: blur(8px);\
\}\
\.miso-flow__node-toolbar button,\
\.miso-flow__edge-toolbar button {\
\  border: none;\
\  background: transparent;\
\  color: var(--mf-panel-color);\
\  font-size: 12px;\
\  padding: 4px 8px;\
\  border-radius: 6px;\
\  cursor: pointer;\
\}\
\.miso-flow__node-toolbar button:hover,\
\.miso-flow__edge-toolbar button:hover { background: var(--mf-accent-soft); }"
-----------------------------------------------------------------------------
themeResizer :: MisoString
themeResizer = "\
\.miso-flow__resize-control.handle {\
\  border: 1.5px solid var(--mf-accent);\
\  background: var(--mf-handle);\
\  border-radius: 2px;\
\}\
\.miso-flow__resize-control.line { border-color: transparent; }"
-----------------------------------------------------------------------------
