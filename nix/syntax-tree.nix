{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraint, containers, deepseq, fetchFromGitHub, hashable
, lens, pretty, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "0f2acyif0583iaxqv9v2vvkf8ygwbmwsxw8qy7vq2xj0c5gw3yhs";
    rev = "8501c027b77f87d9c021ae9c4f08e571976510e8";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraint
    containers deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
