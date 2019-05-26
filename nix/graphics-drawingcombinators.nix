{ mkDerivation, base, bitmap, bitmap-opengl, fetchFromGitHub, freetype-gl
, OpenGL, stb-image, stdenv, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "graphics-drawingcombinators";
    sha256 = "0j8iiymqp10z8zm08c5056a8czcvp5h4kqcfnw9yql8zpdxv7kk3";
    rev = "2c2ffef583656529db66512e2ecdd164b80748b9";
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
