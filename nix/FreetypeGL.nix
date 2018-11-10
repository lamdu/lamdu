{ mkDerivation, base, bindings-freetype-gl, bytestring, directory
, fetchgit, GLFW-b, OpenGL, stdenv, text, transformers
}:
mkDerivation {
  pname = "freetype-gl";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/FreetypeGL";
    sha256 = "1hq27372690p5rycb0wkbl2q6d5rg1qxcgiv96xybqa1jcxcxz74";
    rev = "d2312f89cdf1412d584d13cda235ef683fa9c832";
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
