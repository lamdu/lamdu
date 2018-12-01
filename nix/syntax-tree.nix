{ mkDerivation, base, base-compat, base16-bytestring, binary
, bytestring, constraint, containers, deepseq, fetchFromGitHub, hashable
, lens, pretty, QuickCheck, transformers, stdenv
}:
mkDerivation {
  pname = "syntax-tree";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "syntax-tree";
    sha256 = "1r81pwjc0iy16hjfalsmxaghdxmiha3bjp1r475awbai19s5xzsk";
    rev = "318cede067a076ba31836afd36a9b59791230682";
  };
  libraryHaskellDepends = [
    base base-compat base16-bytestring binary bytestring constraint
    containers deepseq hashable lens pretty QuickCheck transformers
  ];
  homepage = "https://github.com/lamdu/syntax-tree.git#readme";
  description = "Library for typed ASTs";
  license = stdenv.lib.licenses.bsd3;
}
