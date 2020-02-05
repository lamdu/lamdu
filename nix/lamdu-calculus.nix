{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraints, containers, deepseq, fetchFromGitHub
, generic-data, hashable, lattices, lens, monad-st, mtl, pretty
, QuickCheck, hypertypes, transformers, stdenv
}:
mkDerivation {
  pname = "lamdu-calculus";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "lamdu-calculus";
    sha256 = "12m7i0zfrlmi0g0rkm1m1in49sljplikygbj9h9bpdjv05hbzkw8";
    rev = "d18945135657961614c58ac3a38028d54ab4ef1d";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraints containers
    deepseq generic-data hashable lattices lens monad-st mtl pretty QuickCheck hypertypes transformers
  ];
  homepage = "https://github.com/lamdu/lamdu-calculus.git#readme";
  description = "The Lamdu Calculus programming language";
  license = stdenv.lib.licenses.bsd3;
}
