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

      # An allowlist, so editing docs/ or .github/ does not force a rebuild.
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

          # Every non-test dependency is a GHC boot library, so nothing is
          # fetched from Hackage.
          pawl = pkgs.lib.pipe (pkgs.haskell.packages.ghc9141.callCabal2nix "pawl" source { }) [
            pkgs.haskell.lib.compose.doBenchmark
            # -Werror.
            (pkgs.haskell.lib.compose.enableCabalFlag "pedantic")
            (pkgs.haskell.lib.compose.overrideCabal (old: {
              # nixpkgs installs executables but not benchmarks, so the binary
              # would otherwise be discarded.
              postInstall = (old.postInstall or "") + ''
                install -D -m 755 dist/build/pawl-benchmark/pawl-benchmark "$out/bin/pawl-benchmark"
              '';
            }))
          ];
        in
        {
          default = pawl;

          # Its own derivation: inside the build, a cache hit would silently
          # skip the run and a flaky measurement would fail the build.
          benchmark = pkgs.runCommand "pawl-benchmark-results" { } ''
            mkdir -p "$out"
            ${pawl}/bin/pawl-benchmark --csv "$out/bench.csv" +RTS -T
          '';
        }
      );

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

          # From the source root, so ormolu reads default-extensions from
          # pawl.cabal.
          ormolu = pkgs.runCommand "pawl-ormolu-check" { nativeBuildInputs = [ pkgs.ormolu ]; } ''
            cd ${source}
            find source -name '*.hs' -print0 | xargs -0 ormolu --mode check
            touch "$out"
          '';

          # Pinned to the dev shell's version, since cabal-gild's output changes
          # between them. From the source root, for the `discover` directives.
          gild =
            pkgs.runCommand "pawl-gild-check"
              { nativeBuildInputs = [ pkgs.haskellPackages.cabal-gild_1_8_4_1 ]; }
              ''
                cd ${source}
                cabal-gild --mode check pawl.cabal
                touch "$out"
              '';

          # --hint because .hlint.yaml is not in source; -j because hlint is
          # single-threaded by default, which costs 67s against 24s here.
          hlint = pkgs.runCommand "pawl-hlint-check" { nativeBuildInputs = [ pkgs.hlint ]; } ''
            cd ${source}
            hlint --hint=${./.hlint.yaml} -j"$NIX_BUILD_CORES" source
            touch "$out"
          '';

          nixfmt = pkgs.runCommand "pawl-nixfmt-check" { nativeBuildInputs = [ pkgs.nixfmt ]; } ''
            nixfmt --check ${./flake.nix}
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
