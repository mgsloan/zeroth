#!/usr/bin/env runhaskell
> import Distribution.ZeroTH
> main = zeroTHCabalMain (Just ["Data.Derive"]) ["--hashes"] ["Language/Haskell/TH/ZeroTH/GetOpt.hs"]

