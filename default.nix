{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
{
  variant1 = import ./variant1 { inherit nixpkgs compiler; };
  variant1-lens = mport ./variant1-lens { inherit nixpkgs compiler; };
}