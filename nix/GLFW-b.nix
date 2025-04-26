{ mkDerivation, base, bindings-GLFW, deepseq
, HUnit, test-framework, test-framework-hunit
, fetchFromGitHub, stdenv
}:
mkDerivation {
  pname = "GLFW-b";
  version = "3.3.9.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "GLFW-b";
    sha256 = "TODO";
    rev = "506abf792cfd98f9a4e6d0bc526b74f848dc413e";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bindings-GLFW deepseq
  ];
  executableHaskellDepends = [
    base bindings-GLFW deepseq HUnit test-framework test-framework-hunit
  ];
  license = stdenv.lib.licenses.bsd3;
}
