{ mkDerivation, base, bindings-DSL, fetchgit, freetype2, GLEW
, stdenv, freetype-gl, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-freetype-gl";
    sha256 = "08mz6qc9mwlh52knklddxm29bqhkhnc7z9ahfgp7cr8jm1ijbn6d";
    rev = "5a16cfb95490b3dbb019ff27a28ba4b63218d056";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 ];
  librarySystemDepends = [ GLEW freetype ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = stdenv.lib.licenses.bsd3;
}