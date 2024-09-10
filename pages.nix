# nix derivation to build mbbs project site
{
  pkgs ? import <nixpkgs> { },
  gitignoreSource,
}:
with pkgs;
stdenv.mkDerivation {
  name = "pages";
  src = gitignoreSource ./.;

  nativeBuildInputs = [ pandoc ];

  buildPhase = ''
    ${pandoc}/bin/pandoc docs/README.md \
          --from=markdown \
          --to=html \
          --output=index.html \
          --standalone
  '';

  installPhase = ''
    mkdir -p $out
    cp index.html $out
  '';
}
