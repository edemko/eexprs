# Eexprs

This is a collection of hackage packages related to eexprs:

  * `eexpr-core`: the type of eexprs, related types, and some fundamental operations on them
  * `eexpr-bindings`: bindings to the eexpr parser C library; marshals outputs from C into Haskell

If you build with `eexpr-bindings`, you will need to ensure that `libeexpr.so` is on your library path.
If not, you will need to:

  * First, go to the `<this repo>/c` and run `./build.sh` there.
    That will create the C library that `eexpr-bindings` links against.
  * When building: add `--extra-lib-dirs="$PWD/../c/bin/shared"`.
  * When running: define `LD_LIBRARY_PATH="$PWD/../c/bin/shared"`.


