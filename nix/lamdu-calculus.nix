{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchFromGitHub, hashable, lens, pretty
, QuickCheck, syntax-tree, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "lamdu-calculus";
    sha256 = "09w3rsnavr78cfjgv3l5jw6nnvm7bakf66fd7hx774gj9ll58hai";
    rev = "8513ec4e5c2a95be46451b5ac7f048f2bd9273b1";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty QuickCheck syntax-tree transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
