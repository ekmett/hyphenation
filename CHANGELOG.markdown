0.8.2 [2021.06.28]
------------------
* `Embed` flag is now `True` by default to resolve unexpected issues with standalone applications [#18](https://github.com/ekmett/hyphenation/issues/18)

0.8.1 [2021.02.17]
------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.8 [2019.05.18]
----------------
* Per-language default hyphenation minimums
* Correct handling of UTF-8 language definitions, such as Russian.
* `loadHyphenator` now takes a `Language` rather than the string for its affix.
* Updated language files.
* Added Armenian, Assamese, and Occitan.

0.7.1 [2018.01.18]
------------------
* Add `Semigroup` instances for `Exceptions` and `Patterns`.
* Fix the build with `-fembed`.

0.7
---
* Updated Spanish and Thai hyphenation patterns.
* Removed Lao patterns until higher quality patterns are available.
* Updated the licenses associated with many patterns
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.6
---
* Applied `gzip` to the internal data files. This shrinks the resulting library and any statically linked executable by about a meg and a half.

0.5
-----
* Added support for classic Latin and Georgian. Updated language patterns to match `hyph-utf8` version #687 2014-10-13.

0.4.2.1
-------
* `filepath` 1.4 support

0.4.2
-----
* Fixed a typo in .cabal which prevented the `Embed` flag from working properly.

0.4.1
-----
* Added support for embedding all of the pattern files into the library as resources by using `cabal install hyphenation -fembed`. This is not the default as it inflates the library size by ~3MB and forces all users to pay for all the hyphenation patterns, but it can be useful for users who aim to build standalone applications.

0.4
---
* Removed Farsi. We had no pattern files.

0.3
---
* Added Friulan, Piedmontese, Romansh and Thai language hyphenations.

0.2.2
-----
* Flagged `Text.Hyphenation.Language` as `Trustworthy`. It has benign side-effects that cause it to read hyphenation files lazily from the installed `data-dir` but nothing else.
