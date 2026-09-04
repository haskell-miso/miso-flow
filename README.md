# miso-flow

Node-based UIs for [miso](https://github.com/dmjio/miso), powered by
[xyflow](https://github.com/xyflow/xyflow) — the engine behind React Flow
and Svelte Flow.

Drag nodes (and whole subflow groups), pan and zoom, draw connections
between named handles under a custom validation rule, reconnect edges
by their anchors, resize nodes, multi-select with modifier keys or a
shift-drag selection box, float toolbars next to nodes and edges,
delete with the keyboard, snap to a grid, and navigate a pannable
minimap that recenters on click — with the graph itself living in your
miso model as ordinary Haskell data.

## Architecture

`@xyflow/system` splits cleanly into a pure algorithmic core and a set of
imperative DOM gesture modules. miso-flow mirrors that split:

- **Ported to Haskell** — the type layer, edge path math (bezier, smooth
  step, straight — byte-identical SVG paths), viewport math, graph
  utilities, changes, and the store algorithms (`adoptUserNodes`,
  absolute positions, parent expansion). These live in `Miso.Flow.Types`,
  `Miso.Flow.Utils.*`, `Miso.Flow.Changes`, `Miso.Flow.Constants` and are
  usable directly from `View` code.
- **Driven through JavaScript** — the gesture modules (`XYPanZoom`,
  `XYDrag`, `XYHandle`, `XYResizer`, `XYMinimap`) run as the real
  `@xyflow/system` code, bundled from `ts/miso-flow.ts` into
  `js/miso-flow.js`. `Miso.Flow.Internal.Bridge` binds to it; everything
  crossing the boundary is JSON.
- **The MVU layer** — `Miso.Flow.View` renders the DOM contract the
  gesture system expects (pane, transformed viewport, nodes, handles,
  edges); `Miso.Flow` wires bridge callbacks into an `update` function
  and packages the whole thing as a reusable `Component`.

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso (App, defaultEvents, startApp)
import Miso.String (MisoString)
import Miso.Types (text)
import Miso.Flow

main :: IO ()
main = startApp defaultEvents app

app :: App (FlowModel MisoString ()) (FlowAction MisoString ())
app = flowComponent defaultStoreOptions text id nodes edges
  where
    nodes = [ node "1" (xy 0 0) "hello", node "2" (xy 250 100) "world" ]
    edges = [ (edge "e1-2" "1" "2") { edgeAnimated = True } ]
```

`js/miso-flow.js` must be loaded on the page before the app starts (it
defines `globalThis.MisoFlow`):

```html
<script src="miso-flow.js"></script>
<script src="app.js"></script>
```

See `example/` for **Patchwork**, a modular-synth-style patch editor
that exercises every feature: typed modules with rail colors, a
connection rule (`fsValidateConnection` — sources can't feed the main
out directly), a subflow group, named handles, all four edge path
types, reconnectable cables, node and edge toolbars, minimap,
snap-to-grid, and keyboard deletion.

### Styling

Styles ship split, like xyflow's `base.css` / `style.css`:
`flowBaseCSS` is the structural contract the gesture system needs;
`flowThemeCSS` is the default look (light, plus a dark palette behind a
`dark` class), driven by `--mf-*` custom properties. `flowCSS` is both.
To restyle, keep the base and override the tokens — or replace the
theme entirely.

## Building

The library builds with native GHC (≥ 9.12), the GHC JavaScript backend,
and the GHC WASM backend.

```sh
# type-check natively
cabal build lib:miso-flow

# browser build (JS backend)
cabal build exe:miso-flow-example \
  --with-ghc=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
cp dist-newstyle/build/javascript-ghcjs/ghc-*/miso-flow-*/x/miso-flow-example/build/miso-flow-example/miso-flow-example.jsexe/all.js \
   example/static/app.js
cp js/miso-flow.js example/static/
# then serve example/static

# WASM build (loader page in example/static-wasm)
make wasm && make serve-wasm
```

Or use the `Makefile`: `make js`, `make wasm`, `make native`,
`make bridge`, `make serve`.

### Tests

The pure ports are golden-tested against the real TypeScript:
`tests/golden/gen.ts` runs `@xyflow/system` (and the framework packages'
`applyChanges` / `getSimpleBezierPath`) over a grid of inputs and dumps
`tests/golden/golden.json`; the `miso-flow-golden` suite replays those
inputs through the Haskell ports and demands identical output — SVG
paths byte-for-byte, numbers exactly.

```sh
make test     # run the suite
make golden   # regenerate fixtures from ./xyflow, then run it
```

The interactive layer is covered by a Playwright suite
(`tests/browser/patchwork.spec.mjs`, 22 assertions) that drives the
example with real mouse and keyboard input; it passes against both the
JS-backend and WASM builds.

### Rebuilding the JS bridge

The bundle is built from `ts/miso-flow.ts` against the `xyflow`
submodule/checkout in `./xyflow`:

```sh
bun install
bun run build     # or `bun run prod` for a minified bundle
```

## License

BSD-3-Clause. The port follows `@xyflow/system`
(MIT License, © 2019-2024 webkid GmbH).
