{
  inputs = {
    claude-code-nix.url = "github:sadjow/claude-code-nix";
    hooky.url = "github:tfausak/hooky-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      claude-code-nix,
      hooky,
      nixpkgs,
      ...
    }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      inherit (nixpkgs.lib) fileset;

      # An allowlist rather than a whole-tree copy, so that editing docs/,
      # script/ or .github/ does not change the derivation and force a rebuild.
      source = fileset.toSource {
        root = ./.;
        fileset = fileset.unions [
          ./CHANGELOG.md
          ./LICENSE.txt
          ./README.md
          ./cabal.project
          ./data
          ./pawl.cabal
          ./source
        ];
      };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          # Every non-test dependency is a GHC boot library, so this builds
          # without fetching anything from Hackage. doBenchmark compiles the
          # benchmark too, which `cabal test` would not.
          default = pkgs.haskell.lib.compose.doBenchmark (
            pkgs.haskell.packages.ghc9141.callCabal2nix "pawl" source { }
          );
        }
      );

      # Duplicates of CI's lint jobs, so that one `nix flake check` covers what
      # the pipeline covers. They are cheap, and running them before the build
      # means a lint failure costs seconds rather than a full compile.
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          cabal = pkgs.runCommand "pawl-cabal-check" { nativeBuildInputs = [ pkgs.cabal-install ]; } ''
            cd ${source}
            cabal check
            touch "$out"
          '';

          # Run from the source root so ormolu finds pawl.cabal and picks up
          # default-extensions, as it does in the dev shell.
          ormolu = pkgs.runCommand "pawl-ormolu-check" { nativeBuildInputs = [ pkgs.ormolu ]; } ''
            cd ${source}
            find source -name '*.hs' -print0 | xargs -0 ormolu --mode check
            touch "$out"
          '';
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              claude-code-nix.packages.${system}.default
              hooky.packages.${system}.default
              pkgs.bash
              pkgs.cabal-install
              pkgs.coreutils
              pkgs.fzf
              pkgs.gh
              pkgs.git
              pkgs.haskell.compiler.native-bignum.ghc9141
              pkgs.haskellPackages.cabal-gild_1_8_4_1
              pkgs.hlint
              pkgs.jq
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.ripgrep
              pkgs.xdg-utils
            ];

            shellHook = ''
              if test -d .git
              then hooky install
              fi
            '';
          };
        }
      );
    };
}
