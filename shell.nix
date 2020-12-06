{ pkgs ? import <nixpkgs> { }, compiler ? "default", shell ? true }:

let
  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};
in
  haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.overrideCabal drv (attrs: {
        buildTools = (attrs.buildTools or [  ]) ++ [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
        ];
      });
    returnShellEnv = shell;
  }
