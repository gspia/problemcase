
# problem case

To build with GHC:


```
nix-shell -A shells.ghc
cabal new-build all
```

To build with GHCJS:

```
nix-shell -A shells.ghcjs
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

GHCI: `:l app-wai/Main.hs` and then `:reload` and see the browser.

