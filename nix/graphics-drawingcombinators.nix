{ mkDerivation, base, bitmap, bitmap-opengl, fetchFromGitHub, freetype-gl
, OpenGL, stb-image, stdenv, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "graphics-drawingcombinators";
    sha256 = "0h7ffgv37gxjqj6cfmkf5w0y0zgy49zfvc2ss4gyy91rk4z72km3";
    rev = "82f70c03a632402eb32d9e3bbb3fdd9a32492bd5";
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
