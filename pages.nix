# nix derivation to build mbbs project site
{ 
  self,
  pkgs ? import <nixpkgs> { },
  gitignoreSource,
}:
with pkgs;

let commit = if (self ? rev) then self.rev else "dirty";
in

stdenv.mkDerivation {
  name = "pages";
  src = gitignoreSource ./.;

  nativeBuildInputs = [ pandoc ];

  buildPhase = ''
    ${pandoc}/bin/pandoc docs/README.md \
          --from=markdown \
          --to=html \
          --output=index.html \
          --standalone \
          --template=docs/template.html \
          --variable=gitcommit:${commit}
  '';

  installPhase = ''
    mkdir -p $out
    cp index.html $out
  '';
}
