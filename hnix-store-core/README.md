hnix-store-core
=================

Core effects for interacting with the Nix store.

See `StoreEffects` in [System.Nix.Store] for the available operations
on the store.

[System.Nix.Nar]: ./src/System/Nix/Nar.hs
[System.Nix.Path]: ./src/System/Nix/Path.hs
[System.Nix.Store]: ./src/System/Nix/Store.hs


Tests
======

 - `ghcid --command "cabal repl test-suite:format-tests" --test="Main.main"`
