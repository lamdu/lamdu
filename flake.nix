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

          # haskell.nix materialized cabal files often refer to Apple frameworks as
          # `pkgs."CoreFoundation"`, `pkgs."Cocoa"`, etc. Newer nixpkgs exposes
          # them under `pkgs.darwin.apple_sdk.frameworks.*`, so we provide aliases.
          (final: prev:
            prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
              CoreFoundation = prev.darwin.apple_sdk.frameworks.CoreFoundation;
              Cocoa = prev.darwin.apple_sdk.frameworks.Cocoa;
              IOKit = prev.darwin.apple_sdk.frameworks.IOKit;
              CoreVideo = prev.darwin.apple_sdk.frameworks.CoreVideo;
              OpenGL = prev.darwin.apple_sdk.frameworks.OpenGL;
              Security = prev.darwin.apple_sdk.frameworks.Security;
              Foundation = prev.darwin.apple_sdk.frameworks.Foundation;
              AppKit = prev.darwin.apple_sdk.frameworks.AppKit;
              CoreServices = prev.darwin.apple_sdk.frameworks.CoreServices;
            }
          )

          (final: _prev: {
            Lamdu = final.haskell-nix.hix.project {
              name = "Lamdu";
              src = ./.;
              evalSystem = system;
              compiler-nix-name = "ghc984";

              # nixpkgs no longer provides an AGL framework package, but
              # haskell.nix still picks it up via bindings-GLFW on darwin.
              # Override the frameworks list to a known-good set.
              modules = [
                ({ pkgs, lib, ... }: {
                  packages.bindings-GLFW.components.library.frameworks = lib.mkForce (
                    with pkgs.darwin.apple_sdk.frameworks;
                    [
                      Cocoa
                      IOKit
                      CoreVideo
                      OpenGL
                    ]
                  );
                })
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
