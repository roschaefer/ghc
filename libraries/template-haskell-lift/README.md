# `template-haskell-lift`

The stable home of Template Haskell's `Lift` typeclass, which implements cross-stage persistence for Template Haskell.

## Interface stability

We want to ensure that the interace is the same between different versions of GHC. 
At present we check this by comparing the output from the Haddock Hoogle output.

Ideally we'd want to use something like [print-api](https://github.com/Kleidukos/print-api) but that doensn't support all the versions of GHC that we support.


