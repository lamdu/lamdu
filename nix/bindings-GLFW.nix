{ mkDerivation, base, bindings-DSL, fetchgit, hsc2hs, stdenv
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.2.1.2";
  src = fetchgit {
    url = "https://github.com/lamdu/bindings-GLFW";
    sha256 = "0lnqaf5p1y6qxl9ik5pia9p39ipsgqipmxqkc6ih1ma488knl7rm";
    rev = "1498d95a72d24d272a285ea64337bb907f8d89b2";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends = [ hsc2hs ];
  license = stdenv.lib.licenses.bsd3;
}
