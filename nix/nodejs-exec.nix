{ mkDerivation, base, fetchFromGitHub, stdenv, nodejs-6_x, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "nodejs-exec";
    sha256 = "1r3y7k06sg1nwc1fvbr5ps4q8ymg6iaq4gfmkvjaavjvazw5dqff";
    rev = "44d5b7b58801481638520f52d23fb3c95c2428f0";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs-6_x which ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
