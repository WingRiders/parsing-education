.PHONY: all build test clean blueprints

all: build

build:
	cabal build all

test:
	cabal test all

clean:
	cabal clean
	rm -rf artifacts/*

blueprints:
	cabal run blueprints
