{ stdenv, fetchFromGitHub, mesa, pkgconfig, cmake, freetype, glew, doxygen, glfw3, fontconfig, imagemagick, anttweakbar, expat }:

stdenv.mkDerivation rec {
  name = "FreetypeGL-2017-08-10";

  buildInputs = [ mesa pkgconfig cmake freetype glew doxygen glfw3 fontconfig imagemagick anttweakbar expat ];

  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "freetype-gl";
    rev = "d5bc473468422f2cc536bab6e6a87a9aabb7ea07";
    sha256 = "0pz7zbfxn227zp2zgw4f97fin2iamabkdbkpyapp32nbmpw41az8";
  };

  /* postPatch = "cd build"; */
  installPhase = ''
    mkdir -p $out/lib/
    cp ./libfreetype-gl.a $out/lib/
  '';

  meta = {
    description = "A C OpenGL Freetype engine";
    longDescription = ''
      A small library for displaying Unicode in OpenGL using a single texture
      and a single vertex buffer
    '';
    homepage = https://github.com/lamdu/freetype-gl;
    license = stdenv.lib.licenses.bsd2;
    maintainers = [ stdenv.lib.maintainers.razvan ];
    platforms = stdenv.lib.platforms.linux;
  };
}
