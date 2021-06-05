{ mkDerivation
, base
, directory
, lib
, mtl
, parsec
, tasty
, tasty-hunit
}:
mkDerivation {
  pname = "fuspel-compiler";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl parsec ];
  executableHaskellDepends = [ base mtl parsec ];
  testHaskellDepends = [ base directory parsec tasty tasty-hunit ];
  description = "A compiler for the Fuspel language";
  license = lib.licenses.gpl2Only;
}
