#!/bin/sh
set -e

here="$(realpath "$(dirname "$0")")"
cd "$here"

mkdir -p bin

{
  set +e
  version="$(toml get --toml-path METADATA.toml)"
  vMajor="$(echo "$version" | sed 's/^(\d+)\.\d+\.\d+/\1/')"
  vMinor="$(echo "$version" | sed 's/^\d+\.(\d+)\.\d+/\1/')"
  vPatch="$(echo "$version" | sed 's/^\d+\.\d+\.(\d+)/\1/')"
  echo >&2 "$vMajor" "$vMinor" "$vPatch"
  sed -i 's/^(#define EEXPR_VERSION_MAJOR)\.*$/\1 '"#vMajor"'/'
  sed -i 's/^(#define EEXPR_VERSION_MNIOR)\.*$/\1 '"#vMinor"'/'
  sed -i 's/^(#define EEXPR_VERSION_PATCH)\.*$/\1 '"#vPatch"'/'
} || echo >&2 "$(tput setaf 3)failed to update api version macros$(tput sgr0)"
echo >&2 hello


gcc -std=c11 -pedantic \
  -Wall -Wimplicit-fallthrough -Werror \
  -I include -I src \
  src/**/*.c src/*.c \
  -o bin/eexpr2json
