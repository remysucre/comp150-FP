##issues
- haddock: compiled and running, revealed that the strictness is from attoparsec (bytestring)
- bytestring: there is a benchmark suit and a test suit
  - trying to get benchmark to work, but got [typing error](http://stackoverflow.com/questions/31431798/cabal-benchmark-bytestring-package-has-no-benchmark): [`BenchAll.hs`](https://github.com/remysucre/comp150-FP/blob/master/benchmark/profile/bytestring/bench/BenchAll.hs)
  - building with `test` involves lots of deoended packages, which depend on other packages...
- genectic: 
  - selection based on time: change to space
  - abandon once runs loger than base: simulate anealing?
- profiling: 
  - how does the call to `rnf` work [here](https://github.com/remysucre/comp150-FP/tree/master/gentest/hsleak/t1)?
  - why doesnt fixb work
  - what does manually tick mean
- partition: 
  - will try searching for `BangPatterns`, `seq` etc. 

##todo
- compile
  - [ ] bench bytestring
  - [ ] put in SCC
  - [ ] flip bangs and try
- partition
  - [ ] search for `BangPatterns`
  - [ ] ghc/cabal pkg to get package info
  - [ ] add seq...
- profile
  - [ ] run genetic
  - [ ] profile small examples
  - [ ] euler

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

##further project
- better parser: support CPP
- better cabal profiling
- machine learning language 
