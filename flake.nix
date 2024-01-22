{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages = {
        inherit (pkgs) cutechess;

        bayesian-elo = with pkgs; stdenv.mkDerivation rec {
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
      };
    }
  );
}
