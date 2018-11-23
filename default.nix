{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
{
  variant1 = import ./variant1/default.nix { inherit nixpkgs compiler; };
  variant1-lens = import ./variant1-lens/default.nix { inherit nixpkgs compiler; };
}