{
  description = "Parsing Plutarch contracts";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    cores = "1";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";

    haskell-nix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    iohk-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs";

    CHaP.url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    plutarch.url = "github:plutonomicon/plutarch-plutus?ref=gh-pages";
    plutarch.flake = false;

    # pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # This is for CI, for users who does not use Hercules CI, this input
    # along with ./nix/hercules-ci.nix can be removed.
    # hercules-ci-effects.url = "github:hercules-ci/hercules-ci-effects";
  };

  outputs = inputs@{ flake-parts, nixpkgs, haskell-nix, iohk-nix, CHaP, plutarch, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # ./nix/pre-commit.nix
        # ./nix/hercules-ci.nix
      ];
      debug = true;
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];

      perSystem = { config, system, ... }:
        let
          pkgs =
            import haskell-nix.inputs.nixpkgs {
              inherit system;
              overlays = [
                haskell-nix.overlay
                iohk-nix.overlays.crypto
                iohk-nix.overlays.haskell-nix-crypto
              ];
              inherit (haskell-nix) config;
            };
          project = pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc966";
            index-state = "2024-10-09T22:38:57Z";
            inputMap = {
              "https://chap.intersectmbo.org/" = CHaP;
              "https://plutonomicon.github.io/plutarch-plutus/" = plutarch;
            };
            shell = {
              # shellHook = config.pre-commit.installationScript;
              withHoogle = true;
              withHaddock = true;
              exactDeps = false;
              tools = {
                cabal = { };
                haskell-language-server = { };
              };
            };
          };
          flake = project.flake { };
        in
        {
          inherit (flake) devShells packages checks;
        };
    };
  }
