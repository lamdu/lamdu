{ mkDerivation, fetchgit
, base, aeson, bytestring, edit-distance-vector, hashable
, mtl, scientific, text, unordered-containers, vector
, stdenv
}:
mkDerivation {
  pname = "aeson-diff";
  version = "1.1.0.5";
  src = fetchgit {
    url = "https://github.com/thsutton/aeson-diff.git";
    sha256 = "09j0fzsh9dg14xfi3q4il62n8f1ljzd7mhv4ijvrdbgvz5k9ragm";
    rev = "ec68a6edccff549bc980fdaf51b03e8de59d8953";
  };
  libraryHaskellDepends = [
    base aeson bytestring edit-distance-vector hashable
    mtl scientific text unordered-containers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
