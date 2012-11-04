.PHONY=all doc test compile configure clean
default_goal: test

all: clean configure compile test doc

doc: configure
	cabal haddock

test: compile
	cabal test

compile:
	cabal build

configure:
	cabal configure --enable-tests

clean:
	cabal clean
	-rm -rf dist
