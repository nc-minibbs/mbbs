---
title: MBBS data
---

The most recent data for
for the [Mini-Bird Breeding Survey](https://minibbs.us/)
is available at the links below:

* [mbbs_stops_counts.csv](data/mbbs_stops_counts.csv)
* [mbbs_route_counts.csv](data/mbbs_route_counts.csv)
* [surveys.csv](data/surveys.csv)
* [comments.csv](data/comments.csv)
* [route_stop_coordinates.csv](data/route_stop_coordinates.csv)
* [log.txt](data/log.txt)

For a description of these files
and more details on how they are generated,
see the [data pipeline](data-pipeline.html#Data-products).
The [data checklist](data-checklist.html) outlines
how the data are updated annually.

## Versioned Data (recommended)

The above links provide data at the most recent commit.
If you would like to get the data in a reproducible way,
you may use one of the following methods:

### `Nix`

**This assumes you have some knowledge of [`nix`](https://nixos.org/)**

The project's nix flake provides the `#data` output,
which is simply all the files listed above.
The following is an example
[nix flake](https://wiki.nixos.org/wiki/Flakes)
that includes `R` and the mbbs data
in a development environment.
Note the `CHANGEME` in the `mbbs.url` line.
Change `ref` (or `rev`) to a particular git commit or git tag.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    mbbs.url = "github:nc-minibbs/mbbs?ref=CHANGEME";
  };

  outputs = { self, nixpkgs, flake-utils, mbbs }:
    flake-utils.lib.eachDefaultSystem (system:

      let
          mbbs-data = mbbs.packages.${system}.data;

      in {
        packages.mbbs-data = mbbs-data;

        devShells.default = pkgs.mkShell {
          buildInputs = [
            # data
            mbbs-data

            # R
            pkgs.R
            pkgs.rPackages.readr
          ];
        };
      }
    );
}
```

If you run `nix develop .` using the above flake,
then the following example `R` script will access the mbbs data
in the `nix` store.

```r
library(readr)

stop_counts <- read_csv(
  file.path(
    system("nix path-info .#mbbs-data", intern = TRUE)[1],
           "mbbs_stops_counts.csv"),
  col_types = cols(
    year = col_integer(),
    county = col_factor(),
    route = col_factor(),
    route_num = col_integer(),
    stop_num = col_integer(),
    source = col_skip(),
    common_name = col_character(),
    sci_name = col_skip(),
    count = col_integer())
  )
```

### Github Releases

See data available at
our [Github releases](https://github.com/nc-minibbs/mbbs/releases).
