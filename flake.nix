# https://discourse.nixos.org/t/r-packages-the-renv-library-manager/5881/2   
{
  description = "A basic flake for the mbbs package";
  nixConfig = {
    bash-prompt = "mbbs> ";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.default =  pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [
          pkgs.R
        ];
      }; 

    });
}

