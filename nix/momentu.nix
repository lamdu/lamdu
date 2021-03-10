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
    sha256 = "1sl93fmn8hr65vxhdi6mj9jsk20znfmhglkc5ajpldm1bp3kzg17";
    rev = "505a6237f945139b27af52e5c1288dbdb7d59c65";
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
