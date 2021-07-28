# eexprs

I'm tired of making new parsers for languages, so this is the last one.

Parsing string templates, which themselves may contain arbitrary expressions is fairly difficult, but essential for any language that will deal with text manipulation.
Further, indentation-sensitivity is difficult, but very nice.

## Why?

Lisp has a very nice separation between parser (which creates an sexpr from a bytestream) and grammar (which gives semantics to those sexprs).
The Lisp reader is not so different (ignoring reader macros) from a JSON or XML parser: there's still a step before you get to an abstract syntax tree, but at least you don't have to deal with bytes and characters to understand an sexpr/JSON/XML document.

Most lispers probably think that sexprs have perfectly fine ergonomics as the representation of a programming language.
I think Lisp is lacking, and---to be fair---a lot of people avoid Lisp just because it doesn't look fun to program in.
But then a lisper might still object that sexprs are the price you pay for good metaprogramming.
Well, plenty of languages (Haskell and Rust, to name just two) have good support for homoiconicity and all the advantages that come with it; sexprs are now only necessary if you are planning to make a toy.
Personally, my first programming language was Scheme, so by objecting to sexprs in a programming context, I am either speaking deep truth, or betraying everything I love.

Eexprs define a tree-shaped inductive datatype "eexprs" and a partial mapping from text files to this datatype (a concrete syntax).
Eexprs aim to be completely language-agnostic: they focus only on combination of atoms.
Nevertheless, the concrete syntax supports widely-recognized patterns:
  lists, mappings, indented blocks, dotted projection, to name a few.
To build a specific language on top of eexprs, you still must define a concrete syntax mapping eexprs trees into your language's abstract syntax, but at least _this_ parsing is only tree-rewriting: all the hard work of manipulating text has already been done by eexprs.

Languages built on eexprs can look like anything from C to Python to Lisp to Haskell.
The parser is written in C and has a public interface so that bindings can be written for other languages.
At the moment, the only binding available is one for Haskell, which is also where I experiment.

Ultimately, I want to be able to move very rapidly from an academic description of a new language into a convenient implementation.
(By convenient, I mean that the syntax is reasonably familiar, errors are diagnosed descriptively, but interpreter performance is perhaps still as for a toy, though a mature system could be built on top of the existing implementation.)
Eexprs is the first step of this project.


## To-do List

Sometimes I keep this in a separate file.
Sometimes I try to create an exhaustive list of tasks needed for a 1.0 release.
Not this time.
This list is just known upcoming stuff; items are deleted not checked off.

  * design choices
    * how to set tabstops in the source and automatically re-align without losing those tabstops
    * fat colon `a :: b` and fat chain `a::b` syntax
  * major features
    * pragma extraction
    * mixfixes and synthdots
  * output formats
    * json
    * colorized HTML output
    * hook up to the Language Server Protocol
  * technical debt
    * test indentation and whitespace sensitivity more rigorously
    * (DNR) look at `README`/`TODO` under `c`, `hs`
    * (DNR) `grep -r '\bTODO\|FIXME\b' .`


## A Big Visually-Undifferentiated Mass

One of the reasons I think Lisp is unergonomic is because it offers no landmarks.
Although it's obvious to me, it may not be to others, so this section is for supporting evidence and arguments.

Understanding source code occurs over a noisy channel.
Noise can be introduced by reading quickly, making implicit assumptions, unfamiliarity with language edge-cases, and so on (oh, and also if your eyesight isn't perfect, or your font/color scheme is bad).

There are some slides from [Is natural language a good code from an information-theoretic perspective?](http://www.coli.uni-saarland.de/~vera/goodcode.pdf) which gives some of the groundwork for understanding what makes a code good generally.

As a particular example, consider the NATO alphabet, in which a single letter is transfered using multisyllabic words.
This is an absurd amount of redundancy for a reliable transmission medium, but it makes it so much easier to understand spelling over phones/radio/&c.
I posit that reading quickly (as programmers are both wont and required to do) introduces significant noisiness relative to say reading/writing to disk, and perhaps even comparable to phone noise.
