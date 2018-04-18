{ mkDerivation, base, bitmap, bitmap-opengl, fetchgit, freetype-gl
, OpenGL, stb-image, stdenv, text, transformers
}:
mkDerivation {
  pname = "graphics-drawingcombinators";
  version = "1.7.1";
  src = fetchgit {
    url = "https://github.com/lamdu/graphics-drawingcombinators.git";
    sha256 = "0d0q84rfg5yajlphv5zh5z6pnj789m13z1h9h5393kclnb076fih";
    rev = "83b468a530ff2c3b9ebd6b1fc018714d2a7b4907";
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
