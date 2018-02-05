let config = {
    packageOverrides = pkgs: rec {
        freetype-gl = pkgs.callPackage ./nix/freetype-gl.nix {};
        anttweakbar = pkgs.callPackage ./nix/AntTweakBar.nix {};
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
                AlgoW = self.callPackage ./nix/AlgoW.nix {};
                bindings-freetype-gl = self.callPackage ./nix/bindings-freetype-gl.nix { GLEW = pkgs.glew; freetype-gl = freetype-gl; };
                freetype-gl = self.callPackage ./nix/FreetypeGL.nix {};
                graphics-drawingcombinators = self.callPackage ./nix/graphics-drawingcombinators.nix {};
                imagemagick = pkgs.haskell.lib.doJailbreak super.imagemagick;
                lamdu-calculus = self.callPackage  ./nix/lamdu-calculus.nix {};
                nodejs-exec = self.callPackage ./nix/nodejs-exec.nix {};
                OpenGL = self.callPackage ./nix/OpenGL.nix {};
            };
        };
    };
};
in with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/f607771d0f5.tar.gz") { 
    inherit config;
};

{ lamdu = haskellPackages.callPackage ./nix/lamdu.nix {}; }