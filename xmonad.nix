{ mkDerivation, base, containers, stdenv, X11, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "abes-xmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers X11 xmonad xmonad-contrib
  ];
  homepage = "https://github.com/WhiteAbeLincoln/abes-xmonad";
  description = "Abraham White's xmonad configuration";
  license = stdenv.lib.licenses.mit;
}
