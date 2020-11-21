{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./xmonad.nix { }
