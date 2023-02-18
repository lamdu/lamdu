{ mkDerivation, base, bytestring, data-default, exceptions, filepath, resourcet, transformers
, fetchFromGitHub, hsc2hs, lib, rocksdb
}:
mkDerivation {
  pname = "rocksdb-haskell";
  version = "1.0.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "rocksdb-haskell";
    sha256 = "13xr7ppgsfrgyrzi0l5fzsy84glhysipd6ibc045c7mx4g4iill6";
    rev = "5bac865400c67e7bab0c219ec7327284cb87e348";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring data-default exceptions filepath resourcet transformers
  ];
  librarySystemDepends = [ hsc2hs rocksdb ];
  license = lib.licenses.bsd3;
}
