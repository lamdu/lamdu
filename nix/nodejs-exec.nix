{ mkDerivation, base, fetchFromGitHub, stdenv, nodejs, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "nodejs-exec";
    sha256 = "0whgdd7r9wy4mijq5b2wlv5q3aq5ckjc10ljsajbf3kcs6k6ghqb";
    rev = "1cbc71544fa39d8eeb2f477701715a8a3a6e8a49";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
