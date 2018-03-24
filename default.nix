{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc763" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./game2048.nix { }
