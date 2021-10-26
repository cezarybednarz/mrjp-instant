all:
	stack ghc -- --make src/Main.hs -o main

clean:
	rm src/*.hi
	rm src/*.o
