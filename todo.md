##issues
- greping with `BangPatterns` and `$!` had great result! 200+ packs w/ bang & 300+ w/ `$!`. We can easily search for `seq`, `rnf` etc. similarly. 
- working on bytestring: where to start
  - quickcheck on certain functions
  - resolve dependency and get test to work
  - write own benchmark
- genetic

##todo
- compile
  - [ ] bench bytestring
  - [ ] put in SCC
  - [ ] flip bangs and try
- partition
  - [x] search for `BangPatterns`
  - [x] add seq...
- profile
  - [ ] run genetic, print out gene and map gene to time/space
  - [ ] try to profile only one function w/ quickcheck

##notes: 
- [SCC pragma](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#scc-pragma) 
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

##strategies: 
- use profile/analysis to break down code (prevent premature opt.)
- use genetic etc. for machine learning
- potentially improve each function/module independently according to call graph/profile
- TODO: add a picture here?

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
- [GPC](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html) has pretty rendering, but no call tree
- simulated annealing

##further project
- better parser: support CPP
- better cabal profiling
- machine learning language 
