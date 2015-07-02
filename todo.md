##notes: 
- Partition downloaded files: mk 2 dirs, one w/ strict the other w/o also keep a record of popular libs
- To Partition strict packages: cd to GetApps, then bash < shellcmd strict packs are in Apps/strictapps

##issues: 

- relevant files: 
  - HasBang (getting cpp into here)
  - Cpp (trying out cpp)
  - strictapps (30+ strict files)
  - HasStrict (too many open files)
  - hash (files with hashes)
- new strategy: remove all lines with `#define` `#if` `#else`

http://trac.haskell.org/haskell-src-exts/ticket/27

##questions we can ask about packages
- how many bangs?
- how big
- use what lib

##helpful resources:
- glob lib for shell cmd

##todo: 
- [ ] review & clean up
  - [ ] organize & document repo
  - [ ] cis 194
  - [ ] generate log
- [ ] plan monad practice
  - [ ] papers
  - [ ] books/tutorials
  - [ ] exercises
- [ ] finish partition
  - [ ] too many open files
  - [ ] cpphs options
- [ ] get list of pop lib
- [ ] profile strict code vs non-strict
- [ ] automatic github issue tracking
- [ ] look @ paper
