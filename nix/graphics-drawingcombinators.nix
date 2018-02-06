{ mkDerivation, base, bitmap, bitmap-opengl, fetchgit, freetype-gl
, OpenGL, stb-image, stdenv, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchgit {
    url = "https://github.com/lamdu/graphics-drawingcombinators.git";
    sha256 = "12ka3v9lggng6k5scnkpdxy0gmlkl7hydygdgjkfzz4ykvyv7nv4";
    rev = "adf091973d26e80fac49764f03865f3e38a18c23";
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
