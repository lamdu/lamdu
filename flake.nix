{
  description = "Lamdu: A Next-generation Live Programming Environment";
  # Uses stacklock2nix to manage Haskell dependencies.

  inputs.stacklock2nix.url = "github:cdepillabout/stacklock2nix/v5.2.0";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    {
      self,
      nixpkgs,
      stacklock2nix,
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            stacklock2nix.overlay
            self.overlay
          ];
        }
      );
    in
    {
      overlay = final: prev: {
        lamdu-haskell-stacklock =
          let
            ghc = final.haskell.packages.ghc984;
          in
          # hasklib = final.haskell.lib;
          final.stacklock2nix {
            stackYaml = ./stack.yaml;

            baseHaskellPkgSet = ghc;

            # additionalHaskellPkgSetOverrides = hfinal: hprev: {
            #   language-ecmascript = hasklib.dontCheck hprev.language-ecmascript;
            #   text-trie = hasklib.dontCheck hprev.text-trie;
            #   rocksdb-haskell = hasklib.dontCheck hprev.rocksdb-haskell;
            #   bindings-GLFW = hasklib.dontCheck hprev.bindings-GLFW;
            #   GLFW-b = hasklib.dontCheck hprev.GLFW-b;
            # };

            additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
              final.cabal-install
              final.ghcid
              final.stack
              ghc.haskell-language-server
            ];
          };

        Lamdu = final.lamdu-haskell-stacklock.pkgSet.Lamdu;

        lamdu-haskell-dev-shell = final.lamdu-haskell-stacklock.devShell;
      };

      packages = forAllSystems (system: {
        Lamdu = nixpkgsFor.${system}.Lamdu;
      });

      defaultPackage = forAllSystems (system: self.packages.${system}.Lamdu);

      devShells = forAllSystems (system: {
        lamdu-haskell-dev-shell = nixpkgsFor.${system}.lamdu-haskell-dev-shell;
      });

      devShell = forAllSystems (system: self.devShells.${system}.lamdu-haskell-dev-shell);
    };
}
