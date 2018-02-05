{ mkDerivation, base, bindings-freetype-gl, bytestring, directory
, fetchgit, GLFW-b, OpenGL, stdenv, text, transformers
}:
mkDerivation {
  pname = "freetype-gl";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/FreetypeGL";
    sha256 = "152qaxlzv0zq60z6hvbmrkax07m7iinqfdzvlk6k2wnrd737chhg";
    rev = "286b75894e51b08b7c63f8845f045f66d27e9849";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bindings-freetype-gl bytestring directory OpenGL text
    transformers
  ];
  executableHaskellDepends = [
    base bindings-freetype-gl bytestring GLFW-b OpenGL text
    transformers
  ];
  homepage = "https://github.com/Peaker/freetype-gl#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}