{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchgit, hashable, lens, pretty
, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lamdu/lamdu-calculus.git";
    sha256 = "0skhqpaz2a83q64mydmyksyf26x5h247k0y2q5wb1iqiiw1ays9y";
    rev = "94f91c37acbc4a98b7a150b94cb645f6d955f865";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
