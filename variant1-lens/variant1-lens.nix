{ mkDerivation, base, lens, stdenv, variant1 }:
mkDerivation {
  pname = "variant1-lens";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens variant1 ];
  description = "Optics for @variant1@";
  license = stdenv.lib.licenses.bsd3;
}
