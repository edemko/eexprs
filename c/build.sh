#!/bin/sh
set -e

here="$(realpath "$(dirname "$0")")"
cd "$here"

mkdir -p bin

gcc -std=c11 -pedantic -I src -Wall -Wimplicit-fallthrough -Werror src/*.c -o bin/eexpr
