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
    sha256 = "092fs5858zmx18r1y95j2gjv86lkjlna1drpy6ng4mn2a5b66gy0";
    rev = "edcaff261cf4aaab3b613b11ee2812aa6988306f";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers lens
    monad-st mtl pretty QuickCheck template-haskell th-abstraction transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
