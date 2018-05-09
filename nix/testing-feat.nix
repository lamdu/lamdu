{ mkDerivation, fetchgit
, base, QuickCheck, size-based, testing-type-modifiers
, stdenv
}:
mkDerivation {
  pname = "testing-feat";
  version = "1.0.0.0";
  src = fetchgit {
    url = "https://github.com/JonasDuregard/testing-feat.git";
    sha256 = "05n3lxiylhr7xbq3flvslaswn2fqdl7vqvjqjrmp9knljbs5m5yb";
    rev = "8e60d4dbd73c10d98f5f373c590772df0cc0d3a8";
  }
  libraryHaskellDepends = [
    base QuickCheck size-based testing-type-modifiers
  ];
  license = stdenv.lib.licenses.bsd3;
}