.PHONY=all doc test compile configure clean
default_goal: test

all: clean build test

test:
	cabal configure --enable-tests --enable-library-coverage
	cabal build
	cabal test

build: clean
	cabal configure
	cabal build
	cabal haddock

clean:
	cabal clean
	-rm -rf dist
