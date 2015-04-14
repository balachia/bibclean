all:
	ghc -O2 -rtsopts bibclean.hs

profile:
	make
	ghc -O2 -rtsopts -prof -fprof-auto bibclean.hs


