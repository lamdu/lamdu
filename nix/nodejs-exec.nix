{ mkDerivation, base, fetchFromGitHub, lib, nodejs, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "nodejs-exec";
    sha256 = "17x8gxs2phh1wgy6azl9w9paj81ca4fa3ci0kiml7a927rang7n9";
    rev = "b42c7880973a6eed67dffcfb6641e84de07612f2";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = lib.licenses.bsd3;
}
