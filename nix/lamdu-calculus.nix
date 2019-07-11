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
    sha256 = "1mlbmkrfy03ymbmdimav4j93gnpxvmhghfgxhqjvnnnw6pf0a4db";
    rev = "b4412fe2867086937a96ae38e559106c4b21368c";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraints containers
    deepseq hashable lattices lens monad-st mtl pretty QuickCheck syntax-tree transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
