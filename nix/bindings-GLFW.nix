{ mkDerivation, base, bindings-DSL, fetchgit, hsc2hs, stdenv
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.2.1.2";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-GLFW";
    sha256 = "0zb6yh2yvsh2nwhqnsxj4r48fzsxd8nwd117sy3bj6asf0v6abl8";
    rev = "8407bd91daa13e4c7f4cf0b29c736b038bc5c9de";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends = [ hsc2hs ];
  license = stdenv.lib.licenses.bsd3;
}
