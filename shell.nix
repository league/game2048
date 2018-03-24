{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc763" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
