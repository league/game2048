{ mkDerivation, base, containers, deepseq, HUnit, mtl, parallel
, random, stdenv, test-framework-hunit, test-framework-quickcheck
, test-framework-th, time, transformers, vector
}:
mkDerivation {
  pname = "game2048";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq mtl parallel random time transformers
    vector
  ];
  executableHaskellDepends = [ base time ];
  testHaskellDepends = [
    base HUnit test-framework-hunit test-framework-quickcheck
    test-framework-th
  ];
  license = stdenv.lib.licenses.gpl3;
}
