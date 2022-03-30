{ mkDerivation, base, bitmap, bitmap-opengl, fetchFromGitHub, freetype-gl
, OpenGL, stb-image, lib, text, transformers, vector
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "graphics-drawingcombinators";
    sha256 = "004n5c4xdnabbiji7dh1csrjljxxfhzigl8c8v15jvh6pi1hk16q";
    rev = "39500dd189b0676468848d1f452ac31c7f0ec461";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bitmap bitmap-opengl freetype-gl OpenGL stb-image text
    transformers vector
  ];
  homepage = "http://github.com/luqui/graphics-drawingcombinators";
  description = "A functional interface to 2D drawing in OpenGL";
  license = lib.licenses.bsd3;
}
