First, go to the `<this repo>/c` and run `./build.sh` there.
That will create the C library this Haskell package links against.

Build with `cabal build --extra-lib-dirs="$PWD/../../c/bin/shared`.
Run with `LD_LIBRARY_PATH="$PWD/cbits" cabal run --extra-lib-dirs="$PWD/../../c/bin/shared eexpr-delme`.
