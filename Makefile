# The native GHC and the JS-backend GHC both identify as ghc-9.12.2, so
# they must not share a dist dir (their inplace package registrations
# collide). JS builds go to dist-js.
JS_GHC     = javascript-unknown-ghcjs-ghc
JS_GHC_PKG = javascript-unknown-ghcjs-ghc-pkg
JS_DIST    = dist-js
JSEXE      = $(JS_DIST)/build/javascript-ghcjs/ghc-9.12.2/miso-flow-0.1.0.0/x/miso-flow-example/build/miso-flow-example/miso-flow-example.jsexe

WASM_GHC     = wasm32-wasi-ghc
WASM_GHC_PKG = wasm32-wasi-ghc-pkg
WASM_DIST    = dist-wasm
WASM_BIN     = $(WASM_DIST)/build/wasm32-wasi/ghc-9.12.2.20250327/miso-flow-0.1.0.0/x/miso-flow-example/build/miso-flow-example/miso-flow-example.wasm

.PHONY: all native test golden browser-test js wasm bridge serve serve-wasm clean

all: js

native:
	cabal build lib:miso-flow exe:miso-flow-example

test:
	cabal test miso-flow-golden

# regenerate golden fixtures from the real @xyflow/system, then test
golden:
	bun tests/golden/gen.ts
	cabal test miso-flow-golden

# drive the served example with real input (needs `make serve` running
# and a Chrome at $CHROME or google-chrome-stable)
browser-test:
	bun tests/browser/patchwork.spec.mjs

js: bridge
	cabal build exe:miso-flow-example --builddir=$(JS_DIST) --with-ghc=$(JS_GHC) --with-hc-pkg=$(JS_GHC_PKG)
	cp -v $(JSEXE)/all.js example/static/app.js
	cp -v js/miso-flow.js example/static/miso-flow.js

wasm: bridge
	cabal build exe:miso-flow-example --builddir=$(WASM_DIST) --with-ghc=$(WASM_GHC) --with-hc-pkg=$(WASM_GHC_PKG)
	bun $$($(WASM_GHC) --print-libdir)/post-link.mjs --input $(WASM_BIN) --output example/static-wasm/ghc_wasm_jsffi.js
	cp -v $(WASM_BIN) example/static-wasm/app.wasm
	cp -v js/miso-flow.js example/static-wasm/miso-flow.js

bridge:
	bun run build

serve:
	python3 -m http.server -d example/static 8931

serve-wasm:
	python3 -m http.server -d example/static-wasm 8932

clean:
	rm -rf dist-newstyle $(JS_DIST) $(WASM_DIST) \
	  example/static/app.js example/static/miso-flow.js \
	  example/static-wasm/app.wasm example/static-wasm/ghc_wasm_jsffi.js example/static-wasm/miso-flow.js
