# eexprs

I'm tired of making new parsers for languages, so this is the last one.

Parsing string templates, which themselves may contain arbitrary expressions is fairly difficult, but essential for any language that will deal with text manipulation.
Further, indentation-sensitivity is difficult, but very nice.

## Why?

Lisp has a very nice separation of parsing phases.
First, the source text is consumed and transformed into s-expressions.
However, these s-expressions do not constitute the syntax of Lisp:
  they might be part of an embedded language with its own special forms and semantics.
Instead, the Lisp interpreter examines these s-expressions for special forms which can then be translated (perhaps implicitly) into abstract syntax.

The only hitch is that s-expression syntax is... let's say objectionable.
Certainly people object to it with variously thought-out opinions, but even I---whose very first programming language was Scheme---have some specific complaints.
Essentially, s-expressions are a big visually-undifferentiated mass.
On the other hand, multiple visually-distinct methods of marking structures allows programmers to read code faster without compromising comprehension.
Secondarily, the special syntax (esp. dot for cons, the various quoting ticks/unticks) is geared specifically towards lisps rather than being more general; I think the separation between syntax trees and languages could be cleaner.

As a sidenote, a lisper might object that the parsimony of the s-expression grammar that leads to visual uniformity allows Lisp to be homoiconic and manipulate its own code.
To which I say no: a focus on homoiconicity was a separate genius design move.
I'll admit, s-expressions made it easier to innovate this form of metaprogramming, but several non-sexpr languages have quasiquotation today; it simply takes a little more work to design the syntax.

Eexprs define a tree-shaped inductive datatype "eexprs" and a partial mapping from text files to this datatype (a concrete syntax).
Eexprs aim to be completely language-agnostic: they focus only on combination and separation of atoms.
To build a specific language on top of eexprs, you still must define a concrete syntax mapping eexprs trees into your language's abstract syntax, but at least _this_ parsing is only tree-rewriting: all the hard work of manipulating text has already been done by eexprs.

Ultimately, I want to be able to move very rapidly from an academic description of a new language into a convenient implementation.
(By convenient, I mean that the syntax is reasonably familiar, errors are diagnosed descriptively, but performance is perhaps still as for a toy, though a mature system could be built on top of the existing implementation.)
Eexprs is the first step of this project.


## To-do List

Sometimes I keep this in a separate file.
Sometimes I try to create an exhaustive list of tasks needed for a 1.0 release.
Not this time.
This list is just known upcoming stuff; items are deleted not checked off.

  * design choices
    * choose a name:
      * `nest` I don't really like it
      * `mexpr` evocative, but confusable with historical uses
      * `eexpr` for "elaborate expressions", which is descriptive and thematic
    * quasiquote-supporting syntax (perhaps just steal Haskell's)
  * major features
    * parsing
    * Language Server Protocol
  * documentation
    * what is the algorithm for indentation detection?
    * enumerate all whitespace sensitivity
    * what tokens are ignored?
    * expressional equivalences
  * small enhancements
    * include both start and end locations in `LexResult`s
  * technical debt
    * create and test JSON output (token locations will be tested here)
    * test that token stream locations are contiguous
    * test indentation and whitespace sensitivity more rigorously
    * (DNR) `grep -r '\bTODO\|FIXME\b' src/ app/ test/`

## Alteration

Here's how to edit some things I expect to change.

I'm not solid on which characters should be allowed in atoms.
To edit this, look for the `word` parser in `Text.EExprs.Tokens.Megaparsec.Broad`.
, and `isSymbolChar` in `Text.EExprs.Tokens.Lexer.Recognize`.

So far, I am only allowing UNIX-style linebreaks.
On the one hand, this is inflexible, and should probably be changed.
On the other hand, who wants to allow a corporation to dictate that
    combining source files from multiple sources may result in inconsistent linebreaks?

The set of escape characters that are allowed in strings is given in `stringEscapes` in `Text.EExprs.Tokens.Megaparsec.Broad`.

The set of punctuation characters, is given by `brackets` and `separators` in `Text.EExprs.Tokens.Megaparsec.Broad`.
The valid separators are given in `separators` in `Text.EExprs.Tokens.Megaparsec.Recognize`.
Adjacent separator chars are treated as a single token, though that token may be invalid.
It is important that the set of `separatorChars` agree with `separators`.

Brackets must be single-character, as they may appear together but must be kept separate.
The `brackets` list should be an isomorphism.

## A Big Visually-Undifferentiated Mass

I mentioned above that Lisp is hard to read because it offers no landmarks.
Although it's obvious to me, it may not be to others, so this section is for supporting evidence and arguments.

Understanding source code occurs over a noisy channel.
Noise can be introduces by reading quickly, making implicit assumptions, unfamiliarity with language edge-cases, and so on (oh, and also if your font or color scheme are bad).

There are some slides from [Is natural language a good code from aninformation-theoretic perspective?](http://www.coli.uni-saarland.de/~vera/goodcode.pdf) which give some of the groundwork for understanding what makes a code good generally.

As a particular example, consider the NATO alphabet, in which a single letter is transfered using multisyllabic words.
This is an absurd amount of redundancy for a reliable transmission medium, but it makes it so much easier to understand spelling over the phones/radio/&c.
I posit that reading quickly (as programmers are both wont and required to do) introduces significant noisiness relative to say reading/writing to disk, and perhaps even comparable to phone noise.
