let config = {
    packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                ghc862 = pkgs.haskell.packages.ghc862.override {
                    overrides = self: super: rec {
                        hsc2hs = haskell.lib.dontCheck super.hsc2hs;
                        freetype2 = self.callPackage ./nix/freetype2.nix {};
                        bindings-freetype-gl = self.callPackage ./nix/bindings-freetype-gl.nix {};
                        freetype-gl = self.callPackage ./nix/FreetypeGL.nix {};
                        graphics-drawingcombinators = self.callPackage ./nix/graphics-drawingcombinators.nix {};
                        syntax-tree = self.callPackage ./nix/syntax-tree.nix {};
                        lamdu-calculus = self.callPackage ./nix/lamdu-calculus.nix {};
                        nodejs-exec = self.callPackage ./nix/nodejs-exec.nix {};
                        testing-feat = self.callHackage "testing-feat" "1.1.0.0" {};
                        aeson-diff = pkgs.haskell.lib.doJailbreak (self.callHackage "aeson-diff" "1.1.0.5" {});
                        language-ecmascript = pkgs.haskell.lib.doJailbreak (self.callHackage "language-ecmascript" "0.19" {});
                        ekg-core = pkgs.haskell.lib.doJailbreak (self.callHackage "ekg-core" "0.1.1.4" {});
                        th-abstraction = pkgs.haskell.lib.doJailbreak (self.callHackage "th-abstraction" "0.3.1.0" {});
                    };
                };
            };
        };
    };
};
in with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/4b24ab13d65.tar.gz") {
    inherit config;
};

{
lamdu = pkgs.haskell.packages.ghc862.callPackage ./nix/lamdu.nix {};
}
