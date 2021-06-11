# EExpr Source Tree

The most important files here are `lexer.c`, `postlexer.c`, and `parser.c`.
Each contains the core algorithms for their respective stage of overall parsing.

Any specific choices of characters/strings I have made about the language go into `parameters.c`.
This is so that it is easy to alter the lexer as I learn more about what semantics are available in Unicode, and as Unicode itself changes.

The `types.h` file defines the shapes of tokens and eexprs.

The `engine.*` files define the main support data structure which organizes all the information needed during parsing.
The files under the `lexer/` and `parser/` directories hold files that support data structures used during lexing/parsing respectively.
related to the lexing/parsing algorithm, but which should not go in `lexer.*`.

There's also a `main.c` file which is purely responsible for hooking up the algorithm to the command-line.
The program it defines reads a file, then produces a json representation of the parse tree (and warnings/errors) on stdout.
In addition, it can be configured to dump json representations of relevant state to files at various points.
The `main/` directory holds files only relevant for compiling `main.c`.

The `shim/` directory holds (small, straightforward) implementations of "missing" features of C.
I've decided not to rely on external dependencies (even widespread ones like GNU MP or ICU), since I need very little from those libraries (in terms of both performance and functionality), and external C dependencies are often a pain for users.
