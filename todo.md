##issues
- Is it possible to prove that: 
  - If a Bang improves space efficiency in a certain context, it will improve space efficiency in any context
- created cabal file: a lot of dependencies, are they all necessary?
- gene-time map: 
  - only flips bangs in the first level
  - lots of repetitions, caused by not recognizing the same program text
  - however, statistics shows second bang matters!

##todo
- [ ] write 10 small programs
- genetics
  - [ ] generate gene-time map
  - [ ] hinder ghc optimization by using pairs
- bytestring
  - [ ] linear bytestring test

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
