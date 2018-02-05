{ mkDerivation, base, bytestring, containers, fetchgit, GLURaw
, ObjectName, OpenGLRaw, StateVar, stdenv, text, transformers
}:
mkDerivation {
  pname = "OpenGL";
  version = "3.0.2.0";
  src = fetchgit {
    url = "https://github.com/haskell-opengl/OpenGL.git";
    sha256 = "1f6yy9sd4y3aikb2hn1sr0hxq9kgxvf04f5dsa45gmasphiriv4l";
    rev = "8b220ca3940a5ad49e1b117066c5e9e6c9f8020f";
  };
  libraryHaskellDepends = [
    base bytestring containers GLURaw ObjectName OpenGLRaw StateVar
    text transformers
  ];
  homepage = "http://www.haskell.org/haskellwiki/Opengl";
  description = "A binding for the OpenGL graphics system";
  license = stdenv.lib.licenses.bsd3;
}
