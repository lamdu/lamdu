{ mkDerivation, base, bitmap, bitmap-opengl, fetchFromGitHub, freetype-gl
, OpenGL, stb-image, lib, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "graphics-drawingcombinators";
    sha256 = "05aybqdjd30c4744lc8fr4p0s1jl1wlyhbl1yn9c7f7cnvvjp53f";
    rev = "6253576fdd9233df5c3f43002310f38b0928c2d5";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bitmap bitmap-opengl freetype-gl OpenGL stb-image text
    transformers
  ];
  homepage = "http://github.com/luqui/graphics-drawingcombinators";
  description = "A functional interface to 2D drawing in OpenGL";
  license = lib.licenses.bsd3;
}
