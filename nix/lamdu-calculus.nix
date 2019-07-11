{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub
, generic-data hashable, lattices, lens, monad-st, mtl, pretty
, QuickCheck, syntax-tree, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "lamdu-calculus";
    sha256 = "1rqmxv161mg0k5wh5cyf6yg2rhhw7wgczdmcg39fblyw5n6nh91y";
    rev = "07bc2e6b34a87dcf44d11ffa86b79c497cfa6f8f";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraints containers
    deepseq generic-data hashable lattices lens monad-st mtl pretty QuickCheck syntax-tree transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
