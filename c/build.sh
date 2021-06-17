#!/bin/bash
set -e

here="$(realpath "$(dirname "$0")")"

############ Determine Build Configuration ############

app=1    # build eexpr2json application
debug=1  # ATM, just turns on assert statements
fast=0   # turn off all optimizations
shared=0 # build shared library/application
static=1 # build static library/application

while [ $# != 0 ]; do
  case "$1" in
    # standard configurations
    all) app=1 ; shared=1 ; static=1 ;;
    # turn settings on
    app) app=1 ;;
    debug) debug=1 ;;
    fast) fast=1 ;;
    shared) shared=1 ;;
    static) static=1 ;;
    # turn settings off
    no-app) app=0 ;;
    no-debug) debug=0 ;;
    no-fast) fast=0 ;;
    no-shared) shared=0 ;;
    no-static) static=0 ;;
  esac
  shift
done


############ Setup Building Commands ############

compiler=gcc
langOpts="\
-std=c11 -pedantic \
-Wall -Wextra -Werror \
-Wimplicit-fallthrough \
-Wno-type-limits"
if [ "$fast" == 0 ]; then optzOpts="-O2"; else optzOpts="-O0"; fi
if [ "$debug" == 0 ]; then confOpts="-D NDEBUG"; else confOpts=""; fi

compile="$compiler $langOpts $optzOpts $confOpts"


############ Functions to Build Artifacts ############

function main() {
  # TODO do link-time optimization
  cd "$here"
  updateApiMacros || echo >&2 "$(tput setaf 3)failed to update api version macros$(tput sgr0)"
  if [ $static == 1 ]; then
    echo >&2 "building static lib"
    mkStaticLibrary
    if [ $app == 1 ]; then
      echo >&2 "building statically-linked app"
      mkStaticApp
    fi
  fi
  if [ "$shared" == 1 ]; then
    echo >&2 "building shared lib"
    mkSharedLibrary
    if [ $app == 1 ]; then
      echo >&2 "building dynamically-linked app"
      mkSharedApp
    fi
  fi
}

function mkStaticApp() {
  mkdir -p bin/static
  $compile \
    -I src/app -I src/shim \
    src/shim/*.c src/app/*.c \
    -I src/api -L bin/static -l eexpr \
    -o bin/static/eexpr2json
}

function mkSharedApp() {
  mkdir -p bin/shared
  $compile \
    -I src/app -I src/shim \
    src/shim/*.c src/app/*.c \
    -I src/api -L bin/shared -l eexpr \
    -o bin/eexpr2json-shared
}

function mkStaticLibrary() {
  # TODO link-time optimization
  local objFiles src obj
  mkdir -p bin/static/build/{api,internal,shim}
  # compile each translation unit
  for src in src/shim/*.c src/internal/*.c src/api/*.c; do
    src="${src#src/}"
    obj="${src%.c}.o"
    objFiles+="bin/static/build/$obj "
    $compile \
      -I src/shim -I src/internal -I src/api \
      -c "src/$src" \
      -o "bin/static/build/$obj"
  done
  # perform partial linking on the object files
  gcc -r $objFiles -o bin/static/eexpr.o
  # look for all the external symbols that start with `eexpr_`
  # print them all with a `-G` on front, which are arguments to objcopy
  local externOnly
  externOnly="$(nm -g bin/static/eexpr.o | grep '\beexpr_' | awk '{print "-G "$3}')"
  # objcopy then makes all symbols not listed local/internal
  objcopy $externOnly bin/static/eexpr.o
  # finally, create the static library
  ar rcs bin/static/libeexpr.a bin/static/eexpr.o
}

function mkSharedLibrary() {
  local objFiles src obj
  mkdir -p bin/shared/build/{api,internal,shim}
  # compile each translation unit
  for src in src/shim/*.c src/internal/*.c src/api/*.c; do
    src="${src#src/}"
    obj="${src%.c}.o"
    objFiles+="bin/shared/build/$obj "
    $compile \
      -I src/shim -I src/internal -I src/api \
      -fPIC \
      -c "src/$src" \
      -o "bin/shared/build/$obj"
  done
  # perform partial linking on the object files
  gcc -fPIC \
    -r $objFiles \
    -o bin/shared/eexpr.o
  # look for all the external symbols that start with `eexpr_`
  # print them all with a `-G` on front, which are arguments to objcopy
  local externOnly
  externOnly="$(nm -g bin/shared/eexpr.o | grep '\beexpr_' | awk '{print "-G "$3}')"
  # objcopy then makes all symbols not listed local/internal
  objcopy $externOnly bin/shared/eexpr.o
  # finally, create the shared library
  gcc -shared \
    bin/shared/eexpr.o \
    -o bin/shared/libeexpr.so
}

function updateApiMacros() {
  local version vMajor vMinor vPatch
  version="$(toml get --toml-path METADATA.toml api.version)"
  if [ "$(echo "$version" | wc -l)" != 1 ]; then return 1; fi
  echo "$version" | grep -q "^[0-9]\+\.[0-9]\+\.[0-9]\+" || return 1
  vMajor="$(echo "$version" | sed 's/^\([0-9]\+\)\.[0-9]\+\.[0-9]\+.*$/\1/')"
  vMinor="$(echo "$version" | sed 's/^[0-9]\+\.\([0-9]\+\)\.[0-9]\+.*$/\1/')"
  vPatch="$(echo "$version" | sed 's/^[0-9]\+\.[0-9]\+\.\([0-9]\+\).*$/\1/')"
  sed -i \
    -e 's/^\(#define EEXPR_VERSION_MAJOR\).*$/\1 '"$vMajor"'/' \
    -e 's/^\(#define EEXPR_VERSION_MINOR\).*$/\1 '"$vMinor"'/' \
    -e 's/^\(#define EEXPR_VERSION_PATCH\).*$/\1 '"$vPatch"'/' \
    src/api/eexpr.h
}


############ Call Main ############

main
