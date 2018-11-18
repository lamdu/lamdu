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
    sha256 = "1w6v6ry6lxgzmi412hmdpv3b0x1cnyv32dsk45pa4ypslshki9qj";
    rev = "a5222ed7ff5cb670b707cadb06bfe900339672f7";
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
