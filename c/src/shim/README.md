# EExpr Shim Directory

The `common.*` files include utility functions.
At the moment it only holds `checkOom`, which I use to check the results of memory allocation, and die if the system is out of memory.

I need a very limited ability to perform arbitrary-size integer arithmetic.
Namely, I need only add/sub small numbers, multiply by a small number, and format bigints as decimal strings.
Rather than pull in `libgmp` (or learn how to use it), I just implemented these algorithms myself.

`NUL`-terminated strings have probably caused even more damage than null pointers.
The `str` type and associated functions in `strstuff.*` bundle a length with a bytestring.
This also allows me to have `NUL` bytes inside a string.
I also have a `strBuilder` type for the occasions when I need it (mostly interpreting string literals during lexing).

I need a limited ability to manipulate Unicode.
Namely, I store all strings as UTF-8 (since that is the only encoding eexprs accept), so I need a way to pop the variable-width characters off the front of a string, as well as encode a unicode character `uchar` to a plain bytestring.
I've also added a handful of other unicode-aware functions as I needed them.
Beyond that however, UTF-8 can be usually be "blindly" worked with using plain C string functions.

I find myself using doubly-linked lists a lot (or needing a sequence data structure, and a doubly-linked list will do).
This is implemented in a type-safe, polymorphic way in `dllist.{h,c}`.
See `dllist.h` for usage.
Similarly, I found I needed growing arrays several times, and for this there is `dynarr.*`.
