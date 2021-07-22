# E-Expressions

E-expressions are intended to improve the ergonomics of s-expressions without sacrificing their advantages.
The project was born when I wanted to produce implementations of several languages, but was getting fed up writing parsers over-and-over and never really getting the kind of quality I wanted.
Now my strategy is to implement eexprs once, and build my various language parsers as straightforward pattern-matching over eexprs.
I hope that with this, other who wish to play with programming language semantics will be able to avoid the tedium of pushing characters around and instead get to the semantics more quickly.

E-expressions are not yet stable.
In particular, not even I have gotten experience with them so see where any pain points might be.
Nevertheless, I want to start documenting what constitutes an eexpr early.

  * [Elaborating Sexprs to Eexprs](elaborate.md)
  * Eexpr Reference Grammar (TODO)
  * [Open Questions](open-questions.md)

What does the "e" stand for? I think both elaborate and ergonomic fit.
