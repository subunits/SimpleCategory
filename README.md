
# SimpleCategory (Haskell)

This is an ASCII/Unicode‑compliant minimal representation of core
Category‑Theory constructs implemented in Haskell.

## Features

- `Category` typeclass with identity and composition (`⊚` and ASCII `.`)
- Functor (`FunctorF`)
- Natural transformations (`Nat`)
- A minimal `MonadM` class with ASCII‑safe `bindM` and `returnM`
- Fully self‑contained, no external libraries

## Compatibility

- Works in GHCi or any standard Haskell compiler.
- All Unicode operators have ASCII fallbacks.

## Run

```
ghci SimpleCategory.hs
```

## Purpose

This demonstrates how Haskell natively models categorical structure,
in a way that closely corresponds to mathematical category theory.
