{ system ? builtins.currentSystem }:

let

  rp = import ./reflex-platform {
    inherit system;
    config = {
      allowBroken = true;
    };
  };

in rp.project ({ pkgs, ... }: {
  packages = {
    mountaincar = ./mountaincar;
    spaceinvaders = ./spaceinvaders;
  };

  shells = {
    ghcjs = [
      "mountaincar"
      "spaceinvaders"
    ];
  };
})

