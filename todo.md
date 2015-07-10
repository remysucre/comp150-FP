##todo
- partition
  - [ ] ghc/cabal pkg to get package info
  - [ ] add seq...
- compile
  - [ ] build success info 
  - [ ] sandbox
- profile
  - [ ] euler

##issues: 
- partition
  - setting all macros to true actually works
  - but when compiling we still want the actual pack info
    - from [SO](http://stackoverflow.com/questions/31343246/get-package-version-to-cpp/31343829#31343829)
  - cannot expand #if
  - #include macro
  - last in do block must be exp
- compile
  - tons of packs in .cabal/hackage...
- profile

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
