{ mkDerivation, base, fixplate, stdenv }:
mkDerivation {
  pname = "variant1";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base fixplate ];
  description = "Polymorphic variant for unary type constructors";
  license = stdenv.lib.licenses.bsd3;
}
