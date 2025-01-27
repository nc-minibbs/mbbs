# nix derivation to build mbbs project site
{
  self,
  pkgs ? import <nixpkgs> { },
  gitignoreSource,
}:
with pkgs;

let
  commit = if (self ? rev) then self.rev else "dirty";
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

    ${pandoc}/bin/pandoc docs/data-pipeline.md \
          --from=markdown \
          --to=html \
          --output=data-pipeline.html \
          --standalone \
          --template=docs/template.html \
          --variable=gitcommit:${commit}

    ${pandoc}/bin/pandoc docs/data-checklist.md \
          --from=markdown \
          --to=html \
          --output=data-checklist.html \
          --standalone \
          --template=docs/template.html \
          --variable=gitcommit:${commit}
  '';

  installPhase = ''
    mkdir -p $out
    mkdir -p $out/data
    cp index.html $out
    cp data-pipeline.html $out
    cp data-checklist.html $out
    cp -R ${self.packages.${system}.data}/. $out/data
  '';
}
