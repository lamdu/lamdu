{ mkDerivation, base, fetchgit, stdenv, nodejs-6_x, which }:
mkDerivation {
  pname = "nodejs-exec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/nodejs-exec";
    sha256 = "0fzp136gm7i10drflwdc25cnz8fg902i31f6kczr6x3cvkl1p6ym";
    rev = "3635e2f4270bd5325fbada17af95f46fcde641cf";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ];
  extraLibraries = [ nodejs-6_x which ];
  homepage = "https://github.com/lamdu/nodejs-exec";
  description = "Package providing a nodejs installation";
  license = stdenv.lib.licenses.bsd3;
}
