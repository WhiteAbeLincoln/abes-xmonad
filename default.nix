{ pkgs ? import <nixpkgs> { }, compiler ? "default" }:
let
  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage ./abes-xmonad.nix { }
