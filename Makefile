.PHONY: stylish
stylish:
	find src -name '*.hs' -exec cabal exec stylish-haskell -- -i \{\} \;
