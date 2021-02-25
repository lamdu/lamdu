{ mkDerivation, base, bindings-DSL, bytestring, fetchFromGitHub, file-embed, freetype2, glew
, lib, freetype
}:
mkDerivation {
  pname = "bindings-freetype-gl";
  version = "0.1.0.0";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-freetype-gl";
    sha256 = "0m4jw3d4i92r4fbmgjzv7ygp2jx80v2bf7sa80icya4fxskjlb5w";
    rev = "abcb891cf2ed01edb2d02197ef2b354d6ff43db5";
    fetchSubmodules = true;
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL freetype2 bytestring file-embed ];
  librarySystemDepends = [ glew ];
  homepage = "https://github.com/Peaker/bindings-freetype-gl#readme";
  description = "Haskell bindings and embedding of freetype-gl";
  license = lib.licenses.bsd3;
}
