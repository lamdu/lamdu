{ mkDerivation, fetchFromGitHub, lib
, base, text, binary
}:
mkDerivation {
  pname = "text-trie";
  version = "0.2.5.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "text-trie";
    sha256 = "175b35s3ci1vrqcxph4m31angbsiyjxwsw6bpxfyr4bil45wzv5s";
    rev = "072256fb4884d40536c9a31e15fa60d3330d7c58";
  };
  libraryHaskellDepends = [
    base text binary
  ];
  homepage = "https://github.com/lamdu/text-trie";
  description = "An efficient finite map from Text to values, based on bytestring-trie.";
  license = lib.licenses.bsd3;
}
