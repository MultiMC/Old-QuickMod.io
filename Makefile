# A simple Makefile for running commands and things.

CABAL=.cabal-sandbox/bin/cabal

install: .cabal-sandbox
	${CABAL} install -j --enable-tests . yesod-bin yesod-platform --max-backjumps=-1 --reorder-goals

sandbox: .cabal-sandbox

.cabal-sandbox:
	cabal sandbox init && cabal install cabal-install

dev:
	.cabal-sandbox/bin/yesod devel

clean:
	${CABAL} clean

distclean:
	rm -r .cabal-sandbox cabal.sandbox.config

_PHONY: sandbox install dev clean distclean

