{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub
, generic-data, hashable, lattices, lens, monad-st, mtl, pretty
, QuickCheck, hypertypes, transformers, stdenv
}:
mkDerivation {
  pname = "momentu";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "momentu";
    sha256 = "0p452i3c6cbjbsgzir4mfv5vdaa93vkv6vsn1yy8p4h4km4qdx3w";
    rev = "490a12d49107668ebf1e6f0cdcf90cb7dec41ab8";
  };
  libraryHaskellDepends = [
    aeson base base-compat binary bytestring containers deepseq generic-data GLFW-b
    graphics-drawingcombinators lens mtl OpenGL pretty safe-exceptions stm text time timeit
    unicode-properties base base-compat template-haskell
  ];
  homepage = "https://github.com/lamdu/momentu.git#readme";
  description = "The Momentu purely functional animated GUI framework";
  license = stdenv.lib.licenses.bsd3;
}
