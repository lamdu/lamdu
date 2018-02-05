{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchgit, hashable, lens, pretty
, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/lamdu-calculus.git";
    sha256 = "1lz7fb6rvr7ksjzyxjh736gyi4dvw1zkrdacbjb9jdh1p891v320";
    rev = "753fb81d50abf622da907420f1956ec12542cfa9";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
