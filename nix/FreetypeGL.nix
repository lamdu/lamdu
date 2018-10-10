{ mkDerivation, base, bindings-freetype-gl, bytestring, directory
, fetchgit, GLFW-b, OpenGL, stdenv, text, transformers
}:
mkDerivation {
  pname = "freetype-gl";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/FreetypeGL";
    sha256 = "1nkxbgskl8ldnw7i8bls1kf7a5p8cwph7vrpxqir3ka39g5i1p1n";
    rev = "764e86ad05e8821799148cc1c26b316c0e679acb";
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