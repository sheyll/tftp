.PHONY=test build clean install doc
default_goal: test

test:
	cabal configure --enable-tests --enable-library-coverage
	cabal build
	cabal test

build: clean
	cabal configure
	cabal build --ghc-options=-O2

doc:
	cabal haddock

clean:
	cabal clean
	-rm -rf dist

install: build
	cabal install
