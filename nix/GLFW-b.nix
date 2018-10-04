{ mkDerivation, base, bindings-GLFW-gl, deepseq,
, HUnit, test-framework, test-framework-hunit
, fetchgit, stdenv
}:
mkDerivation {
  pname = "GLFW-b";
  version = "3.2.1.1";
  src = fetchgit {
    url = "https://github.com/lamdu/GLFW-b";
    sha256 = "0h30air8k99bhqck11s817zzx51ppi9fjfskwhjx5bsdiblzqlmf";
    rev = "66135b821a15568eaa906a277644fbc2b54d6de3";
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