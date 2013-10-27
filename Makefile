dot: monads/dot.hs 
	@echo "Making the dot executable"
	@echo "Cleaning temporary files from failed builds"
	@rm -f Monads/*.o
	@rm -f Monads/*.hi
	@echo "Compiling files"
	@ghc Monads/dot.hs 
	@mv ./main ./bin
	@echo "Executable in bin folder"
	@echo "Cleaning object files"
	@rm -f Monads/*.o
	@rm -f Monads/*.hi

clean:
	@echo "Removing all temp files"
	@rm -f *.o
	@rm -f *.hi
	@rm -f Monads/*.o
	@rm -f Monads/*.hi

clean-all:
	@echo "Removing all executables"
	@rm -f ./bin/*
	@make clean

main: Main.hs
	@ghc --make -o ./bin/main Main 
	@make clean

all: 
	@make main
