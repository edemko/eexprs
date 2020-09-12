# nest

I'm tired of making new parsers for languages, so this is the last one.

Parsing string templates, which themselves may contain arbitrary expressions is fairly difficult, but essential for any language that will deal with text manipulation.
Further, indentation-sensitivity is difficult, but very nice.



## Alteration

Here's how to edit some things I expect to change.

I'm not solid on which characters should be allowed in atoms.
To edit this, look for the `word` parser in `Text.Nest.Tokens.Megaparsec.Broad`.
, and `isSymbolChar` in `Text.Nest.Tokens.Lexer.Recognize`.

So far, I am only allowing UNIX-style linebreaks.
On the one hand, this is inflexible, and should probably be changed.
On the other hand, who wants to allow a corporation to dictate that
    combining source files from multiple sources may result in inconsistent linebreaks?

The set of escape characters that are allowed in strings is given in `stringEscapes` in `Text.Nest.Tokens.Megaparsec.Broad`.

The set of punctuation characters, is given by `brackets` and `separators` in `Text.Nest.Tokens.Megaparsec.Broad`.
The valid separators are given in `separators` in `Text.Nest.Tokens.Megaparsec.Recognize`.
Adjacent separator chars are treated as a single token, though that token may be invalid.
It is important that the set of `separatorChars` agree with `separators`.

Brackets must be single-character, as they may appear together but must be kept separate.
The `brackets` list should be an isomorphism.
