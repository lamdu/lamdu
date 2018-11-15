{ mkDerivation, base, bindings-DSL, bytestring, fetchgit, file-embed, freetype2, GLEW
, stdenv, freetype-gl, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-freetype-gl";
    sha256 = "0srz2h73nx98sgywr2iwv14f3qdrzn81gw2w9b271rh6gflq992m";
    rev = "9516f1edb86e79d99aa356614b0d81597ef8e49a";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 bytestring file-embed ];
  librarySystemDepends = [ GLEW freetype ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = stdenv.lib.licenses.bsd3;
}
