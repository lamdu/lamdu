{ mkDerivation, base, bytestring, data-default, exceptions, filepath, resourcet, transformers
, fetchFromGitHub, hsc2hs, lib
}:
mkDerivation {
  pname = "rocksdb-haskell";
  version = "1.0.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "rocksdb-haskell";
    sha256 = "0gl4c32yr4qws25b9pxp9pcx3iy7x1b6g6cf6jd1ajs5rb4nfsr7";
    rev = "b0016acefbb38b1fbe148163f226f4cae15c03c7";
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
