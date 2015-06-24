~~Add how to compile and example call~~

~~Look up <$>~~

~~Parse .cabal file for .hs and useful files~~

Log: time command names

integrate AllHs and ShowLib

finish monad practice

Partition downloaded files: mk 2 dirs, one w/ strict the other w/o
                            also keep a record of popular libs

Issue: Cannot add issue because we are on the forked project
       strange log when downloading
       how to make a directory and download to it
       need to add full filepath to AllHs?

done:  AllHs.sh: shell script to search all .hs files in current dir and its sub
       dirs. 
       Searching cabal file doesn't work too well, since only exposed modules
       are exposed and other .hs files aren't always documented. e.g. 
       http://code.haskell.org/~dons/code/stream-fusion/

       ~~Still need to parse cabal file for popular libs.~~

might be helpful: 
    glob lib for shell cmd
