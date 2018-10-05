{ mkDerivation, base, bindings-DSL, fetchgit, hsc2hs, stdenv
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.2.1.2";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-GLFW";
    sha256 = "1kcgdc1f8b0r23zf3gy68ihlgxgjbdw15by99gwdb1w09sbjk02x";
    rev = "216405c67dcc7783296ef0232d2936ac32341361";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends = [ hsc2hs ];
  license = stdenv.lib.licenses.bsd3;
}
