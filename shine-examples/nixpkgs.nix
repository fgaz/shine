let

  bootstrap = import <nixpkgs> {};

  nixpkgs-src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "725b5499b89fe80d7cfbb00bd3c140a73cbdd97f";
    sha256 = "0xdhv9k0nq8d91qdw66d6ln2jsqc9ij7r24l9jnv4c4bfpl4ayy7";
  };

  ghcjs-dom-src = bootstrap.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "0.9.4.0";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };

  config = { 
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {

          # Many packages don't build on ghcjs because of a dependency on doctest
          # (which doesn't build), or because of a runtime error during the test run.
          # See: https://github.com/ghcjs/ghcjs/issues/711
          ghcjs = pkgs.haskell.packages.ghcjs86.override {
            overrides = self: super: with pkgs.haskell.lib; {

              #http-types       = dontCheck super.http-types;
              #comonad          = dontCheck super.comonad;
              #semigroupoids    = dontCheck super.semigroupoids;
              #lens             = dontCheck super.lens;
              #half             = dontCheck super.half;

              QuickCheck       = dontCheck super.QuickCheck;
              tasty-quickcheck = dontCheck super.tasty-quickcheck;

              shine = doJailbreak (super.shine.overrideDerivation (attrs: {}));

              ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" (ghcjs-dom-src + "/ghcjs-dom-jsffi") {};
              ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" (ghcjs-dom-src + "/ghcjs-dom-jsaddle") {};
              ghcjs-dom = self.callCabal2nix "ghcjs-dom" (ghcjs-dom-src + "/ghcjs-dom") {};

            };
          };

        };
      };
    };
  };

in

  import nixpkgs-src { inherit config; }

