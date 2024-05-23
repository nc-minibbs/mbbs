{
  description = "Mini-bird breeding survey data and R package";
  nixConfig = {
    bash-prompt = "🐦> ";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachDefaultSystem (system: let
      
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
            glue
            assertthat
            yaml
          ];
  
    in {

      packages.mbbs = pkgs.rPackages.buildRPackage {
          name = "mbbs";
          src = gitignoreSource ./.;
          propagatedBuildInputs = mbbsDeps;
        };
      
      packages.default = self.packages.${system}.mbbs;

      devShells.default =  pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [
          pkgs.R
          pkgs.rPackages.devtools
          pkgs.rPackages.usethis
          pkgs.rPackages.languageserver
          pkgs.rPackages.styler
        ] ++ mbbsDeps ;
      }; 

    });
}

