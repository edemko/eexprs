# EExpr Source Tree

The source is segregated into several sub-trees, in order of importance:

  * `api/`: The public-facing interface and its implementation.
    Since it consists of a single header file/translation unit pair, documentation was embedded directly into the header rather than a readme.
  * `internal/`: The core implementation of the lexer, postlexer, and parser.
    It is not to be exposed to consumers of the library.
  * `shim/`: Contains "missing" features of C that you wouldn't normally have to think about in most other high-level languages.
    It is used by `internal/` and `app/`, but isn't meant for you to use directly
      (mostly because it's only just enough to do what I need, and better options are available).
  * `app/`: Code used only for the `eexpr2json` executable.
    It is an example of how to write `eexpr`-based applications, since it does not depend on `internal/`.
    It's dependency on `shim/` is only because that was the fastest way for me to get access to bignum and utf8 implementation;
      if you are writing a binding to eexpr in your favorite language, odds are you have a far more complete bignum/utf8 implementation available already.
