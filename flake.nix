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
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs.lib) fileset;
        in
        {
          # Every non-test dependency is a GHC boot library, so this builds
          # without fetching anything from Hackage. doBenchmark compiles the
          # benchmark too, which `cabal test` would not.
          default = pkgs.haskell.lib.compose.doBenchmark (
            pkgs.haskell.packages.ghc9141.callCabal2nix "pawl" (fileset.toSource {
              root = ./.;
              # An allowlist rather than a whole-tree copy, so that editing
              # docs/, script/ or .github/ does not change the derivation and
              # force a rebuild.
              fileset = fileset.unions [
                ./CHANGELOG.md
                ./LICENSE.txt
                ./README.md
                ./cabal.project
                ./data
                ./pawl.cabal
                ./source
              ];
            }) { }
          );
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
