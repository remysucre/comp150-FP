##issues: 

- Profiling: 
  - building requires huge amount of packages (how to use sandbox to organize?)
  - hard to figure out what input to feed in
  - will quickcheck be helpful in generating legal inputs?
  - alternative benchmark: [nofib](https://github.com/ghc/nofib)

- [lattice2.hs](https://github.com/remysucre/comp150-FP/blob/master/profile/euler/lattice/lattice2.hs): deepseq breaks tail recursion? 

- Category theory - everyone talks about it, worth the time to look at?

- Partition strict: 
  - to define macros require packages installed
    - tried to define all macros (MIN_VERSION) as TRUE, got 2k+ errors. abandon
    - tried removing all macro lines, only 300 errors left :) but since macros introduce multiple modules/clauses, still doesnt quite work
  - 49 packages w/strcit!
  - [log file](https://github.com/remysucre/comp150-FP/blob/master/benchmark/partition.log)
  - [error log](https://github.com/remysucre/comp150-FP/blob/master/benchmark/parse.log)

 - [pop libs](https://github.com/remysucre/comp150-FP/blob/master/benchmark/Cabal/libwfreq)

- Bottom: [Newtype wiki] (https://wiki.haskell.org/Newtype#The_messy_bits)

- Thoughts on parsing/building in scale: 
  - difficult because people like to introduce accents to the language
    - learning all the accents/dialects might well be a separate project. machine learning?

- tried running GMain on length -- not working?

##ideas: 
- don't run until finish, stop when heap grows too big

##questions we can ask about packages
- how many bangs?
- how big
- use what lib

##helpful resources:
- glob lib for shell cmd

##todo: 
- profile project euler

- [ ] profile strict code vs non-strict
- [x] review & clean up
  - [x] organize & document repo
  - [x] cis 194
  - [x] generate log
- [x] get list of pop lib
- [x] finish partition
  - [x] too many open files
  - [x] cpphs options
- [x] look @ paper

##practice
- [ ] pld reading
- [ ] start real world
- [ ] papers
- [ ] books/tutorials
- [ ] exercises

##further problems:
- better parser? (cpp support)
- better cabal? (in resolving dependency)
