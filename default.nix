{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
nixpkgs.stdenv.mkDerivation {
  name = "variant1";
  src = ./.;
  buildInputs =
    [ (import ./variant1 { inherit nixpkgs compiler; })
      (import ./variant1-lens { inherit nixpkgs compiler; })
    ];
  installPhase = ''ls'';
  outputs = [ "variant1" "variant1-lens" ];
}