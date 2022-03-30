{ mkDerivation, base, bytestring, data-default, exceptions, filepath, resourcet, transformers
, fetchFromGitHub, hsc2hs, lib
}:
mkDerivation {
  pname = "leveldb-haskell";
  version = "3.2.1.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "leveldb-haskell";
    sha256 = "108gq08zkdxcr3w733dm97nxvh8gghlpl18vhxwry0a1mnkp4dg4";
    rev = "6f2f0b88fd7197e74cbff92bde94168adce3fb29";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring data-default exceptions filepath resourcet transformers
  ];
  librarySystemDepends = [ hsc2hs ];
  license = lib.licenses.bsd3;
}
