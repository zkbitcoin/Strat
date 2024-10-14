#!/bin/sh
{
  #cabal exec strat-exe -- -d4 -rdebug03
  stack exec ./dist-newstyle/build/aarch64-osx/ghc-9.4.8/Strat-0.1.0.0/x/strat-exe/build/strat-exe/strat-exe -- -n checkersWeb
} 2>&1 | tee output.file
