{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, fetchFromGitHub, lens, monad-st, mtl
, pretty, QuickCheck, template-haskell, th-abstraction, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "19j26fyvrr7df21sp18l7qbmbdwy0wcl1yp08k5b3k24n96c7wk6";
    rev = "d382f40ec7018b418867b97c2f8973bd8216d3f5";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
