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

.PHONY: all native test golden browser-test js wasm bridge serve serve-wasm clean ci-wasm ci-bridge

# CI (inside the flake dev shells; see .github/workflows/deploy.yml):
# `make ci-bridge` in .#ghcjs builds the minified gesture bridge, then
# `make ci-wasm` in .#wasm assembles the WASM demo into ./public
# (js/miso-flow.js is spliced into the build at compile time).
WASM_CABAL = wasm32-wasi-cabal

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

# --enable-shared: the bridge is spliced in via TH, and the wasm
# external interpreter needs dependencies built as shared libraries.
# The splice re-reads js/miso-flow.js only when Bridge.hs recompiles;
# neither cabal (mtimes) nor GHC (source hashes) sees the bridge
# change on its own, so poke both.
wasm: bridge
	touch src/Miso/Flow/Internal/Bridge.hs
	rm -f $(WASM_DIST)/build/wasm32-wasi/*/miso-flow-*/build/Miso/Flow/Internal/Bridge.*
	cabal build exe:miso-flow-example --builddir=$(WASM_DIST) --with-ghc=$(WASM_GHC) --with-hc-pkg=$(WASM_GHC_PKG) --enable-shared
	bun $$($(WASM_GHC) --print-libdir)/post-link.mjs --input $(WASM_BIN) --output example/static-wasm/ghc_wasm_jsffi.js
	cp -v $(WASM_BIN) example/static-wasm/app.wasm

bridge:
	bun run build

serve:
	python3 -m http.server -d example/static 8931

serve-wasm:
	python3 -m http.server -d example/static-wasm 8932

ci-wasm:
	$(WASM_CABAL) update
	$(WASM_CABAL) build exe:miso-flow-example --builddir=$(WASM_DIST) --enable-shared
	rm -rf public
	mkdir -p public
	cp -v example/static-wasm/index.html example/static-wasm/index.js public/
	$(eval ci_wasm_bin=$(shell $(WASM_CABAL) list-bin miso-flow-example --builddir=$(WASM_DIST) | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(ci_wasm_bin) --output public/ghc_wasm_jsffi.js
	cp -v $(ci_wasm_bin) public/app.wasm
	cp -v js/miso-flow.js public/miso-flow.js
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

ci-bridge:
	bun install --frozen-lockfile
	bun run prod

clean:
	rm -rf dist-newstyle $(JS_DIST) $(WASM_DIST) public \
	  example/static/app.js \
	  example/static-wasm/app.wasm example/static-wasm/ghc_wasm_jsffi.js
