Using Automated Strictness on Source Code
==========

BE WARNED: This will produce a LOT of temporary files and may eat up all your resources. Be mindful of where you attempt
to run this.

To use, you will need
- happy-1.19.2
- haskell-src-exts-1.14.0
- temporary-1.1.2.4

All three can be obtained by using `cabal` (make sure to `cabal update`).

To compile
- ghc --make GMain.hs

To run
- Create a file `files.txt` in this directory.
- Write down the paths for every file needed to compile your program, with the main module first.
   For instance, if I want to compile a program `main.hs` with three files `fileA.hs, fileB.hs, dir/fileC.hs`, my `files.txt` is
  
> main.hs  
> fileA.hs  
> fileB.hs  
> dir/fileC.hs

- Run ./GMain
- The last output line of output (barring errors) will contain the location of the main module of the most optimized program.
  `[path: \"files/tmp112/main\", vec: 33]` means that the optimized file is in `./files/tmp112/`
