{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub, hashable
, lens, pretty, QuickCheck, transformers, stdenv
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
    base base-compat base16-bytestring binary bytestring constraints
    containers deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
