{ mkDerivation, aeson, base, bytestring, containers, stdenv
, template-haskell, text, X11, xmonad, xmonad-contrib, abesXmonadConfig ? null
}:
(mkDerivation {
  pname = "abes-xmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers template-haskell text X11 xmonad
    xmonad-contrib
  ];
  homepage = "https://github.com/WhiteAbeLincoln/abes-xmonad";
  description = "Abe's custom xmonad";
  license = stdenv.lib.licenses.mit;
}).overrideAttrs (old: {
  CONFIG_SRC = if (abesXmonadConfig == null) then "{}" else builtins.toJSON abesXmonadConfig;
})
