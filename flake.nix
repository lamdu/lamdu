{
  description = "Lamdu: a programming language designed to be useful and delightful";
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            Lamdu = final.haskell-nix.hix.project {
              name = "Lamdu";
              src = ./.;
              evalSystem = system;
              compiler-nix-name = "ghc9103";

              # GHC 9.10.3 boots with filepath-1.5.4.0, so the filepath < 1.5 constraint
              # (from flag(os-string)=False) is unsatisfiable. Explicitly enable os-string
              # so these packages depend on os-string + filepath >= 1.5.0 instead.
              modules = [
                {
                  packages.unix.components.library.configureFlags = [ "-f os-string" ];
                  packages.directory.components.library.configureFlags = [ "-f os-string" ];
                  packages.file-io.components.library.configureFlags = [ "-f os-string" ];
                  packages.process.components.library.configureFlags = [ "-f os-string" ];
                }
              ];

              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
              };
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.Lamdu.flake { };
      in
      flake
      // {
        legacyPackages = pkgs;

        packages = flake.packages // {
          default = flake.packages."Lamdu:exe:lamdu";
        };
      }
    );

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
