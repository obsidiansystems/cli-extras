{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865;
    ghc884 = nixos2003.haskell.packages.ghc884;
    ghc8102 = unstable.haskell.packages.ghc8102;
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "ci-extras" ./. {}) ghcs
