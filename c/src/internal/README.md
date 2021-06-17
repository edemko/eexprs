# Eexpr Internal Implementations

The most important files for understanding the grammar are `lexer.c`, `postlexer.c`, and `parser.c`.
Each contains the core algorithms for their respective stages of overall parsing.

Any specific choices of characters/strings I have made about the language go into `parameters.c`.
This is so that it is easy to alter the lexer as I learn more about what semantics are available in Unicode, and as Unicode itself changes.

The `types.h` file defines the in-memory representations of tokens and eexprs during parsing.
These are meant to be good for building up tokens/eexprs during parsing, but with no mind paid to minimizing their interface;
  thus, these must remain internal to the library.
Since the user of the parser should not free/move token data (instead leaving that to these internals), that de-initializer is implemented in `types.c`.
The corresponding deinitializer for eexprs is in `api/eexpr.c`, since the user _is_ meant to own eexpr data directly.

The `engine.*` files define the main support data structure which organizes all the internal state needed during parsing.
It also defines some helper functions that allow the stages of parsign to interface with the state more easily.
