#!/bin/sh
{
  if lsof -i :8003; then
    echo "A process is still using the port. Killing it..."
    kill -9 $(lsof -t -i:8003)
  fi

  # Determine the OS and set the command accordingly
  if [ "$(uname)" = "Darwin" ]; then
    # macOS
    stack exec ${HOME}/go/src/blockbook/games/ai/server/Strat/dist-newstyle/build/aarch64-osx/ghc-9.4.8/Strat-0.1.0.0/x/strat-exe/build/strat-exe/strat-exe -- -n checkersWeb
  elif [ "$(uname)" = "Linux" ]; then
    # Linux
    [ -f "/home/pivx/.ghcup/env" ] && . "/home/pivx/.ghcup/env" # ghcup-env
    stack exec ${HOME}/go/src/blockbook/games/ai/server/Strat/dist-newstyle/build/x86_64-linux/ghc-9.4.8/Strat-0.1.0.0/x/strat-exe/build/strat-exe/strat-exe -- -n checkersWeb
  else
    echo "Unsupported OS"
    exit 1
  fi
} > /dev/null 2>&1

