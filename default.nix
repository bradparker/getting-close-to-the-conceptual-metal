let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/19.03/nixos-19.03.172361.cf3e277dd0b/nixexprs.tar.xz";
  }) {};
in
  nixpkgs.haskellPackages.callCabal2nix "getting-close-to-the-conceptual-metal" ./. {}
