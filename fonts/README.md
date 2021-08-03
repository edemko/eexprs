# Fonts for Reading/Writing Eexprs

Inside of strings, e-exprs allow any Unicode character; I know of no font family which supply this full range.
Even outside of strings, there is a wide range of Unicode characters available for symbols.
At time of writing, the "likely" symbol characters number 574, for which I could find no free (as in speech and beer) font that covers the whole range---at least not one which looked any good.

Due to the lack of a sufficiently broad font, I have Frankenstiened my own font together from other free fonts.
Unfortunately, I am not a type designer, so I cannot vouch for the visual uniformity of the glyphs, or even the quality of some individual glyphs that I had to modify.
Nevertheless, the font is monospace and covers the whole range of likely symbols characters.
I have released it under the SIL Open Font License v1.1, which is compatible with the licenses of the constituent fonts.
In brief (though I am not a lawyer and this does not constitute legal advice) the SIL lisence allows you to use and redistribute the font, and also to distribute modified versions of font under the same license terms (share-alike).

## Status

At time of writing, I have just finished assembling a font that covers the entire range of likely symbol characters.
There are of course further steps which must be taken to create a quality font, but I have no plans to do these at the moment.
Perhaps the whim will strike me to git gud at fonts, or perhaps I will meet a helpful traveler on my journeys, but for now I am content with bare functionality.

  - [ ] improved source files and build system
  - [ ] visual confusion testing
  - [ ] visual uniformity testing
  - [ ] optional: ligatures for ASCII variants of Unicode codepoints
  - [ ] expand to cover unlikely characters
    - [ ] assemble characters
    - [ ] visual confusion and uniformity

One thing I do plan on doing soon is adding glyphs for fancy enclosers, just as soon as I write that part of the report.

## Methodology

The base font is Adobe Source Code Pro, which is SIL-licensed.
DevaJu Sans Mono filled in a number of symbols from set theory, mathematical operators, some variant relations, and a wide array of arrows (the arrows in Source Code look particularly ugly to me).
Mathematical Alphanumerics were taken from the `bbold` TeX font (blackboard bold), Felipa (math script), and Unifraktur Maguntia (fraktur).
The Metafont sources for bbold were converted to a PFA font by mftrace.
Hebrew letters for mathematics and some miscellaneous symbols were taken from Noto Sans Mono.

Finally, a number of symbols could not be (i.e. I could not) automatically pasted together from the sources above.
Instead, I copy-pasted parts of glyphs into `AmateurGlyphsForEexprs` and modified them.
This set includes:

  * modified from Source Code Pro:
    several variant greek glyphs,
    many relations and set theory operators, as well as
     per-myriad, reduced Plank's constant
  * modified from DejaVu:
    a handful of missing arrows and circled/boxed operators, and
    extra turnstiles

I used fontforge's scripting api to build some tools to assemble my reference font.
The `examinate.py` script takes a number of font filenames, scans each to determine the sizes of glyphs in each font, and which glyphs are not supported by any of the given fonts.
The `fontenize.py` script builds the Eexpr Reference Mono font from the above fonts as an fontforge `sfd` file.
I then check the font metadata in fontforge (ctrl+shift+f) and generate a ttf file from there (ctrl+shift+g).

## Other Fonts

Of course, there is no special need for the provided Eexpr Reference Mono font.
There are a handful of fonts I came across in my exploration that are high-quality, but unsuitable for free distribution.

  * Symbola: it's not a monospace font, but it is beautiful and covers all the characters that my font does.
    Unfortunately, while free as in beer, it is not free as in speech: it cannot be freely modified, or even redistributed.
  * GNU Unifont: the design goal is to cover a massive portion of Unicode, but that unfortunately means cutting corners on aesthetics.
    Personally, I can't get past the absurd feel of the curly brackets.
  * Everson Mono: good aesthetics, and a wide range of characters (98% coverage of eexprs), but it is released under a 90's-style shareware license.
    Importantly, you have to pay the guy some cash if you even want to distribute documents that use the font, so it's unlikely to be a free font until at least the 22nd century.
