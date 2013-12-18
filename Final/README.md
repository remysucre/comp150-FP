Using Strictness on Source Code
==========

To use, you will need
- happy-1.19.2
- haskell-src-exts-1.14.0
- temporary-1.1.2.4

To compile
- ghc -XBangPatterns GMain.hs

To run
- Create a file "files.txt"
- Write down the path to all files, with the main module first
- ./GMain