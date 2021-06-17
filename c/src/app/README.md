# Eexpr To Json Translation Application

This is an example of an application which uses only the defined eexpr API.
It reads eexprs from a file and converts them into json, including location info.
If there are any errors during parsing, these are also reported in the same json object.
It can also be configured to dump representations between parsing stages as well.

The `json.{h,c}` files contain the bulk of json object formatting,
  whereas `main.c` primarily coordinates the parsing algorithm stages (and the usual main-function stuff).

You might ask yourself "If eexprs are supposed to be such a good data format, why would you want to translate them into json?"

  * Well, the machine-machine interfaces being built today are almost entirely json.
    Odds are that your language supports json in its standard library, or perhaps with a well-known library that "everyone" uses.
    If you don't want to write an FFI to this library (or your language doesn't support defining a C FFI), then at the very least you can convert eexprs to json with this binary, and then consume the data from there.
  * Even then, parsing eexprs is more computationally intensive than parsing json.
    Admittedly, eexprs are (presumably) easier to parse than XML or YAML, since you need only a three-character lookahead to lex, a handful of tokens of lookaround to postlex, and a one-token lookahead to parse.
    Eexprs are meant to be generated b yhumans, but as soon as machine-machine interfaces get involved, json will get you a bit more efficiency.
    That said, compared to using binary interfaces, json and eexprs might look to be about equally inefficient.
