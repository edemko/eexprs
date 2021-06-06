# Eexprs in C

The Haskell implementation has gotten away from me a bit I think.
Also, it's not as (easily) portable between languages as a C version woudl be.
A C version would help me constrain a) memory usage and b) type-level cleverness.

Also, as I go I'll be documenting every grammar rule in comments, since the C version parser be as "readable" as the Haskell combinator version.
I hope these vomments can make it directly into some documentation.

## Building

```
./build.sh
```

The source is compliant C99, but the build script assumes gcc.
It's not hard to edit the script as-needed.

## Structure

I'm working on the lexer right now.
The most important file here is `lexer.c`, which contains everything to detect and consume each token type and produce from errors/warnings.
The next landmark is `parameters.c`, which defines sets of characters (what is allowed in numbers/symbols, how to interpret escape sequences).

All of that is built in `strstuff.h`, which defines a sane string type for my own use, as well as types and functions for handling unicode.
My basic idea is to consume everything as Utf8 (as that is the only valid encoding for eexpr files), but manipulate individual characters as Utf32.
I'm not doing any normalization algorithms, since (without ICU) that's complex, annoying, and actually might need to be avoided in downstream interpreters of eexprs.
