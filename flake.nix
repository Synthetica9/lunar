{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      rust-overlay,
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
      {
        packages = rec {
          inherit (pkgs) cutechess;

          lunar-wasm = pkgs.callPackage ./wasm.nix { };

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

          fastchess =
            with pkgs;
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

              nativeBuildInputs = [ lowdown ];
            });
        };
      }
    );
}
