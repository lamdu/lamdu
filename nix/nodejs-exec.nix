{ mkDerivation, base, fetchgit, stdenv, nodejs-6_x, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/nodejs-exec";
    sha256 = "0zaf0z8845mvpk6dcvfickjx34mh4w92myrf5vnbkk6nsm727qhz";
    rev = "6a8b5c7897b822f523414fe2c0e666ecb8d4ab4b";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs-6_x which ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
