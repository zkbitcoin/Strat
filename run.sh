#!/bin/sh
{

  if lsof -i :8003; then
    echo "A process is still using the port. Killing it..."
    kill -9 $(lsof -t -i:8003)
  fi

  #cabal exec strat-exe -- -d4 -rdebug03
  stack exec ${HOME}/go/src/blockbook/games/ai/server/Strat/dist-newstyle/build/aarch64-osx/ghc-9.4.8/Strat-0.1.0.0/x/strat-exe/build/strat-exe/strat-exe -- -n checkersWeb
#} 2>&1 | tee output.file
} > /dev/null 2>&1
