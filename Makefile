dot: monads/dot.hs monads/main.hs
	@echo "Making the dot executable"
	@echo "Cleaning temporary files from failed builds"
	@rm -f monads/*.o
	@rm -f monads/*.hi
	@echo "Compiling files"
	@ghc monads/dot.hs monads/main.hs
	@mv ./main ./bin
	@echo "Executable in bin folder"
	@echo "Cleaning object files"
	@rm -f monads/*.o
	@rm -f monads/*.hi

clean:
	@echo "Removing all temp files"
	@rm -f *.o
	@rm -f *.hi

clean-all:
	@echo "Removing all executables"
	@rm -f ./bin/*
	@make clean

all: 
	@make dot
