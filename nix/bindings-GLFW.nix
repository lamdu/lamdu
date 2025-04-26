{ mkDerivation, base, bindings-DSL, fetchFromGitHub, hsc2hs, stdenv
, libGL, libX11, libXi, libXrandr, libXxf86vm, libXcursor, libXinerama, libXext
}:
mkDerivation {
  pname = "bindings-GLFW";
  version = "3.3.9.3";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "bindings-GLFW";
    sha256 = "TODO";
    rev = "4402c6b6fee81e59f5931fabe1724233cd0ca08c";
  };
  postPatch = ''
    rm Setup.hs
  '';
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base bindings-DSL ];
  librarySystemDepends =
      [ hsc2hs libGL libX11 libXi libXrandr libXxf86vm libXcursor libXinerama libXext
      ];
  license = stdenv.lib.licenses.bsd3;
}
