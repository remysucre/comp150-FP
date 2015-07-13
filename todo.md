##issues
- Got 1 package w/strict up running! (haddock)
- Looking at code coverage now: 
  - [GPC](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html) has pretty rendering, but no call tree
  - [SCC pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#scc-pragma) is only for 1 func but works fine for now
  - cabal build cannot resolve profile dependency: [rebuild the world?](http://stackoverflow.com/questions/1704421/cabal-not-installing-dependencies-when-needing-profiling-libraries/10397592#10397592)
  - code coverage: tried making inf. loops, but not sure about rebuilding
  - when running latex test/html test does print out code coverage, maybe some library? 

##todo
- compile
  - [x] build success info 
  - [x] sandbox
  - [x] get 1 pack compile & running
  - [ ] profile
- partition
  - [ ] ghc/cabal pkg to get package info
  - [ ] add seq...
- profile
  - [ ] euler

##notes: 
- partition
  - when compiling we still want the actual pack info
    - from [SO](http://stackoverflow.com/questions/31343246/get-package-version-to-cpp/31343829#31343829)
  - #include macro
  - last in do block must be exp

##ICFP wishlist

##goal: 
- get 10 profilable packages
  - small examples
  - hackage
  - nofib
- make genetic faster & better
  - limit code coverage: only one bang, strictness annotation
  - timeout space instead of time
  - ensure code coverage, optimize wider range of input

##ideas: 
- dont run until finish, stop when heap grows too big
- simulated anealing
- simulated quantum state collaps?
- thoughts on parsing/building in scale: 
  - difficult because people like to introduce accents to the language
  - learning all the accents/dialects might well be a separate project. machine learning?

##questions we can ask about packages
- how many bangs?
- how big
- use what lib

##helpful resources:
- glob lib for shell cmd

##further project
- better parser: support CPP
- better cabal profiling
- machine learning language
