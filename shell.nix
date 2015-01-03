# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "cataskell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ fgl fglVisualize aeson MonadRandom randomShuffle lens ];
  testDepends = [ hspec QuickCheck deepseq ];
  buildTools = [ cabalInstall hsdev ];
  shellHook =
    ''
      e() { "/Applications/Sublime Text.app/Contents/MacOS/Sublime Text" $@ & }
    '';
  meta = {
    homepage = "http://github.com/chrisjr/cataskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
