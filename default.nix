let config = {
    packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                ghc843 = pkgs.haskell.packages.ghc843.override {
                    overrides = self: super: rec {
                        AlgoW = self.callPackage ./nix/AlgoW.nix {};
                        bindings-freetype-gl = self.callPackage ./nix/bindings-freetype-gl.nix { GLEW = pkgs.glew; freetype-gl = freetype-gl; };
                        freetype-gl = self.callPackage ./nix/FreetypeGL.nix {};
                        graphics-drawingcombinators = self.callPackage ./nix/graphics-drawingcombinators.nix {};
                        lamdu-calculus = self.callPackage  ./nix/lamdu-calculus.nix {};
                        nodejs-exec = self.callPackage ./nix/nodejs-exec.nix {};
                        testing-feat = self.callHackage "testing-feat" "1.1.0.0" {};
                        language-ecmascript = pkgs.haskell.lib.doJailbreak (self.callHackage "language-ecmascript" "0.17.2.0" {});
                    };
                };
            };
        };
    };
};
in with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/b8ffd2459d6.tar.gz") {
    inherit config;
};

{ lamdu = pkgs.haskell.packages.ghc843.callPackage ./nix/lamdu.nix {}; }
