{ mkDerivation, base, bitmap, bitmap-opengl, fetchgit, freetype-gl
, OpenGL, stb-image, stdenv, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchgit {
    url = "https://github.com/lamdu/graphics-drawingcombinators.git";
    sha256 = "13bx2jpm7ml5x5vyjc3sgkasf8kxyma44pqp084yq0mhafsp5drh";
    rev = "2d44457f0b8385af600289cb1feca4cc03a6e39e";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bitmap bitmap-opengl freetype-gl OpenGL stb-image text
    transformers
  ];
  homepage = "http://github.com/luqui/graphics-drawingcombinators";
  description = "A functional interface to 2D drawing in OpenGL";
  license = stdenv.lib.licenses.bsd3;
}
