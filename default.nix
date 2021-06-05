let
  pkgs = import <nixpkgs> {};
in
pkgs.haskellPackages.callPackage ./nix/default.nix {}
