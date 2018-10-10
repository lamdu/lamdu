{ mkDerivation, base, bindings-DSL, bytestring, fetchgit, file-embed, freetype2, GLEW
, stdenv, freetype-gl, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-freetype-gl";
    sha256 = "1v1lc0c0kv1bllkb1b2kxllvyv67c4jw6jazan65l66yl9nyqpxi";
    rev = "60c8daadacaef620865ca174c0f5c46607b5aab8";
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