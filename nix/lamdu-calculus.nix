{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub, hashable, lattices, lens, monad-st, mtl, pretty
, QuickCheck, syntax-tree, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "lamdu-calculus";
    sha256 = "1xxb1ivx3x01jpw2r5264b88idn3f86vplfiirgg6dwxj3k51gx7";
    rev = "e57d99244f27d9b6d5d7b9ff0002c58bf4a89b0d";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraints containers
    deepseq hashable lattices lens monad-st mtl pretty QuickCheck syntax-tree transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
