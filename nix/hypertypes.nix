{ mkDerivation, base, base-compat, base16-bytestring, binary
, constraints, containers, deepseq, fetchFromGitHub, generic-data, lattices, lens, monad-st
, mtl, one-liner, pretty, QuickCheck, show-combinators, template-haskell, th-abstraction
, transformers, stdenv, generic-constraints
}:
mkDerivation {
  pname = "hypertypes";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "hypertypes";
    sha256 = "1anzr29isjsg7npkr7nn62wm6f657cz2pm2ynghbz0kxf1afq24q";
    rev = "32b8d4e5a804dc638f0c0882d54110c7b15ee616";
  };
  libraryHaskellDepends = [
    base base-compat binary constraints containers deepseq generic-data lattices lens
    monad-st mtl one-liner pretty QuickCheck show-combinators template-haskell
    th-abstraction transformers generic-constraints
  ];
  homepage = "https://github.com/lamdu/hypertypes.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
