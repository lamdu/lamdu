{ mkDerivation, base, fetchgit, stdenv, nodejs-6_x, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/nodejs-exec";
    sha256 = "0i72v71s9gqvsnv0bls604rg10x3prvlsdzb7s8vzb192n515jq2";
    rev = "614cfa868899f2dde5801e1b95d4bc43815e9851";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs-6_x which ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
