#!/bin/bash
set -e

function dieUsage() {
  echo >&2 "usage: $0 [[command] case]"
  # TODO echo >&2 "for more detail: $0 help"
  exit 1
}

if [ $# = 0 ]; then
  command=run
  case=all
elif [ $# = 1 ]; then
  command=run
  case="$1"
elif [ $# = 2 ]; then
  command="$1"
  case="$2"
else
  dieUsage
fi

case "$command" in
  run)
    if [ "$case" = all ]; then
      die 'run all unimplemented'
    else
      die 'run <case> unimplemented'
    fi
    # check properties of the outputs, like that all token locs are in increasing order, or that all blocks have at least one subexpr
    # run it when the output differs from golden
  ;;
  commit)
    die 'commit-golden unimplemented'
  ;;
esac
