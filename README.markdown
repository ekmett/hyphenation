comonad
=======

[![Build Status](https://secure.travis-ci.org/ekmett/comonad.png?branch=master)](http://travis-ci.org/ekmett/comonad)

Configurable Knuth-Liang hyphenation using the UTF8 encoded hyphenation patterns provided by [hyph-utf8](http://www.ctan.org/tex-archive/language/hyph-utf8)

Usage:

```haskell
>>> hyphenate english_US "supercalifragilisticexpialadocious"
["su","per","cal","ifrag","ilis","tic","ex","pi","al","ado","cious"]
```

```haskell
>>> hyphenate english_US "hyphenation"
["hy","phen","ation"]
```

```haskell
>>> hyphenate icelandic "va\240lahei\240avegavinnuverkf\230rageymslusk\250r"
["va\240la","hei\240a","vega","vinnu","verk","f\230ra","geymslu","sk\250r"]

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett

