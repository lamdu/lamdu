{ mkDerivation, fetchgit, stdenv
, base
}:
mkDerivation {
  pname = "freetype2";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/lamdu/freetype2";
    sha256 = "0mg7wknchkmcifnvbar9h73p3pqja72hk6w846zsjnmrwihs2k0f";
    rev = "71941659a29fe353bf975aefeb1805417c8ab364";
  };
  libraryHaskellDepends = [ base ];
  description = "Wrapper around FreeType 2 library.";
  license = stdenv.lib.licenses.bsd3;
}
