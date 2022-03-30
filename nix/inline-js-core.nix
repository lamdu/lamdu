{ mkDerivation, base, fetchFromGitHub, lib, nodejs, which }:
mkDerivation {
  pname = "inline-js-core";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "tweag";
    repo = "inline-js";
    rev = "5da3690abd454e5e94021dcd4486484e89fc419f";
    sha256 = lib.fakeSha256;
  } + "inline-js-core";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs ];
  license = lib.licenses.bsd3;
}
