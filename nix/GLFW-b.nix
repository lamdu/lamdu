{ mkDerivation, base, bindings-GLFW, deepseq
, HUnit, test-framework, test-framework-hunit
, fetchFromGitHub, stdenv
}:
mkDerivation {
  pname = "GLFW-b";
  version = "3.2.1.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "GLFW-b";
    sha256 = "TODO";
    rev = "04b0c6c36f351ce629af6bbe76ff440c40b3ff8c";
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
