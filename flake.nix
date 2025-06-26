{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    poetry2nix.url = "github:nix-community/poetry2nix";
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      poetry2nix,
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        poetry2nix = (inputs.poetry2nix.lib.mkPoetry2Nix { inherit pkgs; });
      in
      {
        packages = rec {
          inherit (pkgs) cutechess;
          inherit (poetry2nix) mkPoetryApplication;

          chess-tuning-tools = mkPoetryApplication rec {
            projectDir = pkgs.fetchFromGitHub {
              # https://github.com/kiudee/chess-tuning-tools
              owner = "kiudee";
              repo = "chess-tuning-tools";
              rev = "v0.9.5";
              hash = "sha256-CeLdU3rkRN7SbbCpicbrSqfyuz7Wo1dUSXmAYpXWGuo=";
            };

            overrides = poetry2nix.defaultPoetryOverrides.extend (
              self: super: {
                # nox-poetry = super.nox-poetry.overridePythonAttrs (old: {
                #   buildInputs = (old.buildInputs or [ ]) ++ [ super.poetry ];
                # });
                bask = super.bask.overridePythonAttrs (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ self.poetry-core ];
                  buildInputs = (old.buildInputs or [ ]) ++ [ self.poetry ];

                  postPatch = ''
                    sed 's/poetry.masonry/poetry.core.masonry/' -i pyproject.toml
                  '';
                });
              }
            );
            python = pkgs.python39;
            extras = [ "dist" ];
            groups = [ "main" ];
            doCheck = false;
            postPatch = ''
              sed 's/poetry.masonry/poetry.core.masonry/' -i pyproject.toml
              sed 's/poetry>=0.12/poetry-core/' -i pyproject.toml
              cat pyproject.toml
            '';
            buildInputs = [
              python.pkgs.poetry-core
              pkgs.poetry
            ];
          };

          bayesian-elo =
            with pkgs;
            stdenv.mkDerivation rec {
              pname = "bayesian-elo";
              version = "0057.1.2";

              src = fetchFromGitHub {
                # https://github.com/ddugovic/BayesianElo
                owner = "fsmosca";
                repo = "BayesianElo";
                rev = version;
                hash = "sha256-RU/AZ2I9Gcyb4yxz1qT8qaZYycs3KjKAZpl5hrcn44U=";
              };

              buildPhase = ''
                cd src
                make
              '';

              installPhase = ''
                ls
                install -Dm555 bayeselo -t $out/bin/
              '';
            };

          fastchess = with pkgs;
          stdenv.mkDerivation (self: {
            pname = "fastchess";
            version = "1.4.0-alpha";

            src = fetchFromGitHub {
              owner = "Disservin";
              repo = self.pname;
              rev = "v${self.version}";
              hash = "sha256-fzNpanfeXk7eKftzcs5MIaDBvzumaMQIhhQ8IDFjwPQ=";
            };

            installPhase = ''
              export PREFIX=$out
              make install
            '';

            nativeBuildInputs = [lowdown];
          });
        };
      }
    );
}
