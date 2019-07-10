let

  pkgs = import ./nixpkgs.nix;

  shine-examples = pkgs.haskell.packages.ghcjs.callCabal2nix "shine-examples" ./. {};

in

  shine-examples.env

