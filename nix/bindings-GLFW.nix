{ mkDerivation, base, bindings-DSL, fetchgit, hsc2hs, stdenv
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.2.1.2";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-GLFW";
    sha256 = "1rd2923nlclilr78028d1sp1vi7y159xmf9z5ixrdz5z6q9zhmx8";
    rev = "b1c31d136b6f5c6d0cc08c7670ef04e1a1e48215";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends = [ hsc2hs ];
  license = stdenv.lib.licenses.bsd3;
}