{ mkDerivation, base, bindings-DSL, bytestring, fetchFromGitHub, file-embed, freetype2, glew
, lib, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-freetype-gl";
    sha256 = "15vqy4zdg4ji6yly6xlvik4247cyr569xgwvklxyqn4sjghy20rz";
    rev = "8d311d96c5fa7fcfb5b79d057c9c24944d5d234b";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 bytestring file-embed ];
  librarySystemDepends = [ glew ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = lib.licenses.bsd3;
}
