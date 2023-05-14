{ mkDerivation, base, fetchFromGitHub, lib, which, inline-c, template-haskell, containers }:
mkDerivation {
  pname = "inline-c-objc";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "inline-c";
    rev = "9af83f947acacc52f10a134fde3931dcbf3f845b";
    sha256 = "todo";
  } + "/inline-c-objc";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base inline-c template-haskell containers ];
  extraLibraries = [];
  license = lib.licenses.bsd3;
}
