let config = {
    packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                ghc925 = pkgs.haskell.packages.ghc925.override {
                    overrides = self: super: rec {
                        rocksdb-haskell = haskell.lib.dontCheck (self.callPackage ./nix/rocksdb-haskell.nix {});
                        freetype2 = self.callPackage ./nix/freetype2.nix {};
                        bindings-freetype-gl = self.callPackage ./nix/bindings-freetype-gl.nix {};
                        freetype-gl = self.callPackage ./nix/FreetypeGL.nix {};
                        graphics-drawingcombinators = self.callPackage ./nix/graphics-drawingcombinators.nix {};
                        hypertypes = self.callPackage ./nix/hypertypes.nix {};
                        text-trie = haskell.lib.dontCheck (self.callPackage ./nix/text-trie.nix {});
                        momentu = self.callPackage ./nix/momentu.nix {};
                        lamdu-calculus = self.callPackage ./nix/lamdu-calculus.nix {};
                        inline-js-core = self.callPackage ./nix/inline-js-core.nix {};
                    };
                };
            };
        };
    };
};
in with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/e3053fc54661dbbdcf4a776e5067e89a92e481d7.tar.gz") {
    inherit config;
};

{
lamdu = pkgs.haskell.packages.ghc925.callPackage ./nix/lamdu.nix {};
}
