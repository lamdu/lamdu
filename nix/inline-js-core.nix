{ mkDerivation, base, fetchFromGitHub, lib, nodejs, which }:
mkDerivation {
  pname = "inline-js-core";
  version = "0.0.1.0";
  src = fetchFromGitHub {
    owner = "tweag";
    repo = "inline-js";
    rev = "8512b09d2c0533a41d5d2aef182b11a58c420c10";
    sha256 = "0l4m99jzmadv8dxcb932l3ddcx3qr1fr98q96dpblb2anbqal20y";
  } + "/inline-js-core";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs ];
  license = lib.licenses.bsd3;
}
