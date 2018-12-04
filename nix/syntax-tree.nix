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
    sha256 = "0qzkrxxl43sf4s76i1syrnjprmqlm9s519402vds033zcl6rqvcr";
    rev = "87a407715d682a68e1f3326aa3bbfd608735db55";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
