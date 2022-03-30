{ mkDerivation, base, bindings-freetype-gl, bytestring, directory
, fetchFromGitHub, GLFW-b, OpenGL, lib, text, transformers
}:
mkDerivation {
  pname = "freetype-gl";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "FreetypeGL";
    sha256 = "1c27bpyx0pl5xw7bwlfrf9nlbkz4k988nipnw0wlllb0dlih4rrw";
    rev = "18ca298d8fd48ad18f75264553b57714bf070c77";
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
  license = lib.licenses.bsd3;
}
