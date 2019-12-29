let
  overloaded-overlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {
        overloaded = self.haskell.lib.doJailbreak hsuper.overloaded;
      };
    };
  };
  nixpkgs = import (builtins.fetchTarball {
    url = https://releases.nixos.org/nixos/unstable/nixos-20.03pre206632.b0bbacb5213/nixexprs.tar.xz;
  }) {
    overlays = [overloaded-overlay];
  };
in
  nixpkgs.haskellPackages.callCabal2nix "hot-air" ./. {}
