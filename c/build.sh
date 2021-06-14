#!/bin/sh
set -e

here="$(realpath "$(dirname "$0")")"
cd "$here"

mkdir -p bin

gcc -std=c11 -pedantic \
  -Wall -Wimplicit-fallthrough -Werror \
  -I include -I src \
  src/**/*.c src/*.c \
  -o bin/eexpr2json
