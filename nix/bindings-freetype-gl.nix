{ mkDerivation, base, bindings-DSL, bytestring, fetchFromGitHub, file-embed, freetype2, glew
, stdenv, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-freetype-gl";
    sha256 = "1sqw8cxz1kqnwl62738qdhwgqdjlzqm0ljr27xaz4702p0f3ybx9";
    rev = "3e409dd8c5232abc53fd10e8138658333b90456f";
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
