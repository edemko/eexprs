# Eexprs in C

The Haskell implementation has gotten away from me a bit I think.
Also, it's not as (easily) portable between languages as a C version would be.
A C version would help me constrain a) memory usage and b) type-level cleverness.

Also, as I go I'll be documenting every grammar rule in comments, since the C version parser be as "readable" as the Haskell combinator version.
I hope these comments can make it directly into some documentation.

[homepage]: https://github.com/Zankoku-Okuno/eexprs
[issue-tracker]: https://github.com/Zankoku-Okuno/eexprs/issues

## Notes to Self

I'm currently working on defining a solid C interface.
It's going in `include/`, but I have not documented that fact.

- [x] eliminate easy `TODO`/`FIXME`s
- [x] create a C interface
- [ ] decide about isSymbolChar
  - [ ] go through ASCII for the allow/denylist
  - [ ] understand unicode character classes
  - [ ] go through mathematical symbols for the allow/denylist
- [x] create tests
  - [x] make test runner
  - [x] every production in the grammar must succeed
  - [x] ungrammatical eexprs should have good error reporting
  - [x] valgrind everything for good measure
  - [ ] fuzz it
  - [ ] sanitize (at least for undefined behavior)
- [ ] check `TODO`/`FIXME` again
- [ ] extra grammar
  - [ ] escape sequences matching `\\^[@-_]` (e.g. `"\^D"` for end-of-transmision)
  - [ ] perhaps I should allow "fat colon" `::` in the grammar (perhaps similar to dot: fat chain vs. fat colon)
- [ ] write a "standards document" for the eexpr grammar

I should consider the next technical step.
I'd like to expose the C API into Haskell and write a wrapper around it.
I think I'd also like to include support for mixfixes, maybe even directly in this library., but probably a prototype in Haskell first.
I do thin kit's important to have mixfixes implemented before I start building programming languages, though I could likely write some markup langs (better html, xcompose database, and importantly, a mixfix specification language).
I think that settles it:

> Create haskell bindings, then implement mixfix rewriting and a mixfix specification language and mixfix compiler.

A related transformation I'd like to do is switching between variants of symbols, esp. ascii vs. unicode.

## Building

The simplest route is:

```
./build.sh
```

which builds a statically-linked executable (and library).
You can pass arguments to enable or disable other artifacts and configurations.
The build script documents what arguments are available.

The source is standard C11, but the build script uses `gcc` because that's what I personally use.
It's not hard to edit the script as-needed.

The current build artifacts are:

  * static library `bin/static/libeexpr.a`
  * statically-linked executable `bin/static/eexpr2json`
    which parses an eexpr file and produces a json description on stdout
  * shared library `bin/shared/libeexpr.a`
  * dynamically-linked executable `bin/static/eexpr2json`
    which acts just like the static one, but I have no idea why you'd want a dynamically-linked version


After building, test with `./test/run.sh` or `./test/run.sh run <case name>`.
Once the actual output is satisfactory, it can easily be made the expected output with `./test/run.sh commit <case name>`.

### Dependencies

There are none (okay, fine, just a C99 or newer compiler).
I've decided not to depend on stuff like GNU MP or ICU, since I really need only a fragment of their functionality (and performance).
Managing dependencies in C is a pain, so I prefer not to if I can get away with it, and I expect users will be the same.

It's entirely possible that it might not compile on a non-gcc compiler.
If so, that's unintentional, and should be [reported as a bug](issue-tracker).


## Contributing

If you run across something you think should parse but doesn't, open an issue and include the misbehaving input.
Same thing if you find something that doesn't parse, but the error isn't correct or descriptive enough.
I really don't care if it's a minimal example, I'll take as many test cases as I can get my hands on.

If you've got some code to contribute, submit a pull request.
Be aware that this project is BSD-3 licensed, so do not open a pull request unless you consent to have your code also under this same license.

The `src` tree contains `README.md` files that explain the structure of each directory.
