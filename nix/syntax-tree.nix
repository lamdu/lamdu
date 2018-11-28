{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, containers, deepseq, fetchFromGitHub, hashable, lens, pretty
, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "1rildsmkq310a9xv2jhh01kgvnk02lx60kl3w16p813br9akm7r3";
    rev = "f11955414a8dd81f28e7483156709fb72c436617";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring containers
    deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
