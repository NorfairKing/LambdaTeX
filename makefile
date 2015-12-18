SOURCES = $(shell find $(SRC_DIR) -type f -name '*.hs')

combinators:
	runghc -isrc src/Util.hs

doc: $(SOURCES)
	cabal haddock --haddock-options="--no-warnings --no-print-missing-docs --pretty-html"

