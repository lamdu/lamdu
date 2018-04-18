{ mkDerivation, base, fetchgit, stdenv, nodejs-6_x, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/nodejs-exec";
    sha256 = "0lynr2kggsnkx34ckddc3y7vviv6ch3z50wp4mr5lvwrmlqq5q25";
    rev = "7c763e1acf9c28d35aae034fdab6ee592ac8031d";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs-6_x which ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
