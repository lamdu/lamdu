{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchgit, hashable, lens, pretty
, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/lamdu-calculus.git";
    sha256 = "1rzqbaiy2ams98blp77sf8li18j21053n4y7039pq1dvhl7248pr";
    rev = "533aca8513575f681053aa1747d0f3c22230b86d";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
