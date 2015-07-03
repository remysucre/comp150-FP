Parsers for .cabal files

UsedLib: lists all libraries imported
compile: bash makef
run: ./UsedLib *.cabal

AllHs: lists all .hs files (including sub-dir)
run: bash AllHs.sh
