{ mkDerivation, base, bindings-GLFW-gl, deepseq,
, HUnit, test-framework, test-framework-hunit
, fetchgit, stdenv
}:
mkDerivation {
  pname = "GLFW-b";
  version = "3.2.1.1";
  src = fetchgit {
    url = "https://github.com/lamdu/GLFW-b";
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
