let config = {
    packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                ghc902 = pkgs.haskell.packages.ghc902.override {
                    overrides = self: super: rec {
                        hsc2hs = haskell.lib.unmarkBroken (haskell.lib.dontCheck super.hsc2hs);
                        universe-reverse-instances = haskell.lib.unmarkBroken (haskell.lib.dontCheck super.universe-reverse-instances);
                        lattices = haskell.lib.unmarkBroken (haskell.lib.dontCheck super.lattices);
                        rocksdb-haskell =
                            haskell.lib.dontCheck
                            ( self.callPackage ./nix/rocksdb-haskell.nix {}
                            );
                        freetype2 = self.callPackage ./nix/freetype2.nix {};
                        bindings-freetype-gl = self.callPackage ./nix/bindings-freetype-gl.nix {};
                        freetype-gl = self.callPackage ./nix/FreetypeGL.nix {};
                        graphics-drawingcombinators = self.callPackage ./nix/graphics-drawingcombinators.nix {};
                        hypertypes = self.callPackage ./nix/hypertypes.nix {};
                        momentu = self.callPackage ./nix/momentu.nix {};
                        lamdu-calculus = self.callPackage ./nix/lamdu-calculus.nix {};
                        inline-js-core = self.callPackage ./nix/inline-js-core.nix {};
                        aeson-diff = self.callHackageDirect { pkg = "aeson-diff"; ver = "1.1.0.12"; sha256 = "QU0iju2gg2zuqhJu6W5mnHF/vBcFAIYR/T6Tvw6ne8o="; } {};
                        lens-aeson =
                            self.callHackageDirect
                            { pkg = "lens-aeson";
                              ver = "1.2.1";
                              sha256 = "r/7vq/YI+E12l+hYwQCGwjQB6eVtJe73GbKLNSxONpA=";
                            } {};
                        ekg-core = haskell.lib.doJailbreak (haskell.lib.dontCheck super.ekg-core);
                        cryptohash-md5 =
                            haskell.lib.dontCheck
                            (self.callHackage "cryptohash-md5" "0.11.100.1" {});
                        cryptohash-sha1 =
                            haskell.lib.dontCheck
                            (self.callHackage "cryptohash-sha1" "0.11.100.1" {});
                        language-ecmascript =
                            haskell.lib.dontCheck (haskell.lib.doJailbreak (self.callHackageDirect
                            { pkg = "language-ecmascript";
                              ver = "0.19.1.0";
                              sha256 = "0mbwz6m9666l7kmg934205gxw1627s3yzk4w9zkpr0irx7xqml5i";
                            } {}) );
                        base16-bytestring =
                            haskell.lib.dontCheck
                            (self.callHackage "base16-bytestring" "1.0.1.0" {});
                    };
                };
            };
        };
    };
};
in with import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/058cbf42eb309763418d10882ce004cc7215968a.tar.gz") {
    inherit config;
};

{
lamdu = pkgs.haskell.packages.ghc902.callPackage ./nix/lamdu.nix {};
}
