{ mkDerivation, base, bindings-DSL, bytestring, fetchFromGitHub, file-embed, freetype2, glew
, stdenv, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-freetype-gl";
    sha256 = "06c87664xdrq1q2dyp9dm7lx8s04cxpcc5i1inj9mhhjf1hyyiz3";
    rev = "c720425fc6ec3b68adfb7b25221723bab9520a9c";
    fetchSubmodules = true;
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 bytestring file-embed ];
  librarySystemDepends = [ glew ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = stdenv.lib.licenses.bsd3;
}
