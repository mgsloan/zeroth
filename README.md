# zeroth for ghc-7.6 and ghc-7.8
TemplateHaskell is fairly useful for generating new
Haskell code. This, however, can incur a dependency on
TH on subsequent uses where none theoretically should exist.
ZeroTH solves this by scanning a file for top-level TH
declarations, evaluates them, removes TH imports, and outputs
the resulting code.

Note that this only includes splices at top-level (those with type `DecsQ`)

# install
A dependency hskeleton won't compile unless you remove some orphan instances.
  
      darcs get http://code.haskell.org/~aavogt/hskeleton_ghc78_fix/
      cd hskeleton_ghc78_fix
      cabal install

Alternatively, just install this repo with `cabal-meta`

      cabal install cabal-meta cabal-src
      cabal-meta install
