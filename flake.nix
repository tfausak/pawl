{
  inputs = {
    cabal-gild.inputs.nixpkgs.follows = "nixpkgs";
    cabal-gild.url = "github:tfausak/cabal-gild-nix";
    claude-code-nix.inputs.nixpkgs.follows = "nixpkgs";
    claude-code-nix.url = "github:sadjow/claude-code-nix";
    hooky.inputs.nixpkgs.follows = "nixpkgs";
    hooky.url = "github:tfausak/hooky-nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs =
    {
      cabal-gild,
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

      source = fileset.toSource {
        root = ./.;
        fileset = fileset.unions [
          ./cabal.project
          ./CHANGELOG.md
          ./data
          ./LICENSE.txt
          ./pawl.cabal
          ./README.md
          ./source
        ];
      };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pawl = pkgs.lib.pipe (pkgs.haskellPackages.callCabal2nix "pawl" source { }) [
            pkgs.haskell.lib.compose.doBenchmark
            (pkgs.haskell.lib.compose.enableCabalFlag "pedantic")
            (pkgs.haskell.lib.compose.overrideCabal (old: {
              postInstall = (old.postInstall or "") + ''
                install -D -m 755 dist/build/pawl-benchmark/pawl-benchmark "$out/bin/pawl-benchmark"
              '';

              # This check phase is the only place CI runs the suite, and
              # without these flags tasty defaults to NoTimeout: a
              # non-terminating regression would run to the platform's job
              # limit instead of failing. Each entry reaches the test binary as
              # `--test-option`, so tasty sees `--timeout 5s`.
              #
              # The figure is a floor against a hang, not an assertion about
              # speed: a real hang never terminates at any budget, so raising
              # or lowering this cannot mask one -- it only sets how long CI
              # waits before naming the case. Measured 2026-08-13 on an
              # unloaded aarch64-darwin: the whole suite runs in 26s and its
              # slowest case not already carrying a `Tasty.localOption` budget
              # in `source/test-suite/Main.hs` takes 1.7s. If a loaded runner
              # ever trips this, bump the number -- that is not a performance
              # regression to chase.
              testFlags = (old.testFlags or [ ]) ++ [
                "--timeout"
                "5s"
              ];
            }))
          ];
        in
        {
          default = pawl;
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

          gild =
            pkgs.runCommand "pawl-gild-check" { nativeBuildInputs = [ cabal-gild.packages.${system}.default ]; }
              ''
                cd ${source}
                cabal-gild --mode check pawl.cabal
                touch "$out"
              '';

          hlint = pkgs.runCommand "pawl-hlint-check" { nativeBuildInputs = [ pkgs.hlint ]; } ''
            cd ${source}
            hlint --hint=${./.hlint.yaml} --threads="$NIX_BUILD_CORES" source
            touch "$out"
          '';

          nixfmt = pkgs.runCommand "pawl-nixfmt-check" { nativeBuildInputs = [ pkgs.nixfmt ]; } ''
            nixfmt --check ${./flake.nix}
            touch "$out"
          '';

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
              cabal-gild.packages.${system}.default
              claude-code-nix.packages.${system}.default
              hooky.packages.${system}.default
              pkgs.bash
              pkgs.cabal-install
              pkgs.coreutils
              pkgs.fzf
              pkgs.gh
              pkgs.ghc
              pkgs.git
              pkgs.hlint
              pkgs.jq
              pkgs.nixfmt
              pkgs.ormolu
              pkgs.ripgrep
              pkgs.xdg-utils
            ];

            shellHook = ''
              if test -d .git && ! test -f .git/hooks/pre-commit
              then hooky install
              fi
            '';
          };
        }
      );
    };
}
