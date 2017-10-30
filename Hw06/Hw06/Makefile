interp : parser.hs
		ghc -o $@ $^

test : interp tests/*
		for t in tests/*; do ./interp $$t; done

clean :
		rm interp
			rm *.hi *.o

.PHONY: test clean<
