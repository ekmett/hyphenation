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
