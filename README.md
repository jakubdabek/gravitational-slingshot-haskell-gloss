# Simulation of a gravitational slingshot in Haskell

## Run with

### Old cabal

```bash
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
$EXECUTABLE [mode] [speed] # mode 'a' for acceleration, 'd' for deceleration, default 'a'
```

### New cabal

```bash
cabal sandbox init
cabal v2-build
$EXECUTABLE [args...]
```
