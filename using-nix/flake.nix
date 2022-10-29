{ inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/release-22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        config = { };

        overlay = pkgsNew: pkgsOld: {
          # spire =
          #   pkgsNew.haskell.lib.justStaticExecutables
          #     pkgsNew.haskellPackages.spire;
          playground =
            pkgsNew.haskell.lib.justStaticExecutables
              pkgsNew.haskellPackages.playground;

          haskellPackages = pkgsOld.haskellPackages.override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              # spire = ./.;
              playground = ./.;
            };
          });
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in rec {
        # packages.default = pkgs.haskellPackages.spire;
        packages.default = pkgs.haskellPackages.playground;

        apps.default = {
          type = "app";

          # program = "${pkgs.spire}/bin/spire";
        };

        # devShells.default = pkgs.haskellPackages.spire.env;
        # devShells.default = pkgs.haskellPackages.playground.env;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          withHoogle = true;
          inputsFrom = [ pkgs.haskellPackages.playground.env ];
        };
      }
    );
}