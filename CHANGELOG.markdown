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
