.PHONY=test executable clean
default_goal: test


test:
	cabal configure --enable-tests --enable-library-coverage
	cabal build
	cabal test

all: clean
	cabal configure
	cabal build
	cabal haddock

clean:
	cabal clean
	-rm -rf dist
