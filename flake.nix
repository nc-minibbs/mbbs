{
  description = "A basic flake for the mbbs package";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # https://discourse.nixos.org/t/r-packages-the-renv-library-manager/5881/2     
      devShells.default =        pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = with pkgs; [ 
          R 
        ];
      }; 
      
      # (pkgs.buildFHSUserEnv {
      #   name = "mbbs";
      #   targetPkgs = pkgs: 
      #     (with pkgs; [
      #       R 
      #       rPackages.renv
      #     ]);
      # });
      
      # pkgs.mkShell {
      #   nativeBuildInputs = [ pkgs.bashInteractive ];
      #   buildInputs = with pkgs; [ 
      #     R 
      #   ];
      # };

    });
}

