{ mkDerivation, base, bindings-DSL, bytestring, fetchFromGitHub, file-embed, freetype2, glew
, stdenv, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-freetype-gl";
    sha256 = "0r2dm2v1gcqmmpcmkajbi2076axlh83k16lj7500dp44fkh7r47g";
    rev = "c178be550fbffe135435478a71fcb1dad537963a";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 bytestring file-embed ];
  librarySystemDepends = [ glew ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = stdenv.lib.licenses.bsd3;
}
