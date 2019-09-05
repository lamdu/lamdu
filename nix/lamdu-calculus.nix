{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub
, generic-data, hashable, lattices, lens, monad-st, mtl, pretty
, QuickCheck, syntax-tree, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "lamdu-calculus";
    sha256 = "0kyd7p0s3gf03k176rbiqb324axv1sx1bphiyxw7ydv7j9hz0i1k";
    rev = "f0876c96d25197c35b4ecbeaa27fc6033fac6baf";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraints containers
    deepseq generic-data hashable lattices lens monad-st mtl pretty QuickCheck syntax-tree transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
