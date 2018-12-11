{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, lens, monad-st, mtl
, pretty, QuickCheck, template-haskell, th-abstraction, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "0mhac6chy4sal73aashldl335wj3avlqb2l2ldcrfi7kzx4kc3y7";
    rev = "ea5fb08a022c575c4d44e4ce5012a925c88374b0";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
