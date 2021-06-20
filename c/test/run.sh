#!/bin/bash
set -e

here="$(dirname "$(realpath "$0")")"
cd "$here"

function dieUsage() {
  echo >&2 "usage: $0 [[command] case]"
  # TODO echo >&2 "for more detail: $0 help"
  exit 1
}

function runCase() {
  local case="$1"
  local oldWD="$PWD"
  cd "cases/$case/"
  echo >&2 "$(tput bold)running test: $case$(tput sgr0)"
  if [ ! -f "README.md" ]; then
    echo >&2 "$(tput bold)$(tput setaf 3)[WARNING]$(tput sgr0) no description (README.md) for case $case"
  fi
  if [ ! -x "./run.sh" ]; then
    echo >&2 "$(tput setaf 6)[SKIP]$(tput sgr0) no test (run.sh) for case $case"
    cd "$oldWD"
    return 0
  fi
  for f in ./*.output; do if [ -f "$f" ]; then rm "$f"; fi; done
  ./run.sh 1>stdout.output 2>stderr.output
  local ec=0
  local gold out
  for gold in ./*.golden; do
    gold="${gold#./}"
    if [ "$gold" = "*.golden" ]; then continue; fi
    if [ ! -f "${gold%.golden}.output" ]; then
      echo >&2 "$(tput setaf 1)[FAIL]$(tput sgr0) missing ${gold%.golden}.output"
      ec=1
    fi
  done
  for out in ./*.output; do
    out="${out#./}"
    if ! diff -q 2>/dev/null "${out%.output}.golden" "$out"; then
      echo >&2 "$(tput setaf 1)[FAIL]$(tput sgr0) output differs from expected: $out"
      ec=1
    fi
  done
  cd "$oldWD"
  return $ec
}

function commitCase() {
  local case="$1"
  local oldWD="$PWD"
  cd "cases/$case/"
  local gold out
  for gold in ./*.golden; do
    gold="${gold#./}"
    if [ "$gold" = "*.golden" ]; then continue; fi
    rm "$gold"
  done
  for out in ./*.output; do
    out="${out#./}"
    if [ "$out" = "*.output" ]; then continue; fi
    mv -v "$out" "${out%.output}.golden"
  done
  cd "$oldWD"
}

###### Main ######

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
      ec=0
      for case in cases/*; do
        case=${case#cases/}
        if [ "$case" = "*" ]; then continue; fi
        runCase "$case" || ec=1
      done
      exit $ec
    else
      runCase "$case"
    fi
    # check properties of the outputs, like that all token locs are in increasing order, or that all blocks have at least one subexpr
    # run it when the output differs from golden
  ;;
  commit)
    if [ "$case" = all ]; then
      die 'committing all outputs to golden unimplemented'
    fi
    commitCase "$case"
  ;;
esac
