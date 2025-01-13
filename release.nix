{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  commonOverrides = self: super: {
    which = self.callHackageDirect {
      pkg = "which";
      ver = "0.2";
      sha256 = "sha256-1n6UurMdVAQ6kdQoz0BT6HA2WEoHsKOZN+xYM7Av6bw=";
    } {};
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = commonOverrides;
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = commonOverrides;
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "cli-extras" ./. {}) ghcs
