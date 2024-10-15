{
  description = "Mini-bird breeding survey data and R package";
  nixConfig = {
    bash-prompt = "🐦> ";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      gitignore,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let

        pkgs = nixpkgs.legacyPackages.${system};
        inherit (gitignore.lib) gitignoreSource;

        mbbsDeps = with pkgs.rPackages; [
          beepr
          readxl
          tidyr
          purrr
          stringr
          lubridate
          readr
          magrittr
          dplyr
          tidyr
          beepr
          glue
          assertthat
          yaml
          logger
        ];
      in
      {

        formatter = pkgs.nixfmt-rfc-style;

        packages.mbbs = pkgs.rPackages.buildRPackage {
          name = "mbbs";
          src = gitignoreSource ./.;
          propagatedBuildInputs = mbbsDeps;
        };

        ## WIP!!
        packages.data = with pkgs; stdenv.mkDerivation {
          name = "data";
          version = "";
          src = gitignoreSource ./.;
          buildInputs = [
            R
            self.packages.${system}.mbbs
          ] ++ mbbsDeps;
          buildPhase = 
          ''
           ${R}/bin/Rscript --vanilla -e 'd <- mbbs::create_mbbs_counts(); write.csv(d$stop_level, file = "mbbs_stops_counts.csv", row.names = FALSE); write.csv(d$route_level, file = "mbbs_route_counts.csv", row.names = FALSE)'
          '';
          installPhase = ''
            mkdir -p $out
            cp mbbs_stops_counts.csv $out
            cp mbbs_route_counts.csv $out
          '';

        };

        packages.pages = import ./pages.nix {
          self = self;
          pkgs = pkgs;
          gitignoreSource = gitignoreSource;
        };

        packages.default = self.packages.${system}.mbbs;

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = [
            pkgs.R
            pkgs.rPackages.devtools
            pkgs.rPackages.usethis
            pkgs.rPackages.languageserver
            pkgs.rPackages.styler
            pkgs.rPackages.jsonlite
          ] ++ mbbsDeps;
        };
      }
    );
}
