{ mkDerivation, fetchgit
, base, dictionary-sharing, testing-type-modifiers, template-haskell
, stdenv
}:
mkDerivation {
  pname = "sized-functors";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/JonasDuregard/sized-functors.git";
    sha256 = "1qhlax6il65z0i2kdy33pfysy4ry0xa158i5zg6fcsh2l90iksyl";
    rev = "54e0218211138c153e9cc9c2e62ddfbaf4d9690e";
  };
  libraryHaskellDepends = [
    base dictionary-sharing testing-type-modifiers template-haskell
  ];
  license = stdenv.lib.licenses.bsd3;
}