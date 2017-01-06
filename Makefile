HC=ghc --make

Main: *.hs
	$(HC) $@

clean:
	rm Main || true
	rm *.o || true
	rm *.hi || true
