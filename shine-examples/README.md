# shine-examples

## Setup

This project is configured using the `reflex-platform`, as explained in the [Project Development documentation](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md). 

To install and run the `shine-examples` project:

- Fetch the `reflex-platform` submodule.

```
$ git submodule update --init --recursive
```

- See the [reflex-platform OS Compatibility notes](https://github.com/reflex-frp/reflex-platform#os-compatibility).

- If you've never built a project with `reflex-platform` before, run the `try-reflex` script.

```
$ reflex-platform/try-reflex
...
[nix-shell]$ exit
```

- Build the project.

```
$ nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"
```

- Open the generated pages in a browser, for example,

```
$ firefox dist-ghcjs/build/x86_64-linux/ghcjs-8.6.0.1/mountaincar-0.1/x/mountaincar/build/mountaincar/mountaincar.jsexe/index.html 

$ chromium dist-ghcjs/build/x86_64-linux/ghcjs-8.6.0.1/spaceinvaders-0.1/x/spaceinvaders/build/spaceinvaders/spaceinvaders.jsexe/index.html 
```

## Description of the examples

### Mountaincar

This is a human-controlled version of the [Mountain car problem](https://en.wikipedia.org/wiki/Mountain_car_problem) (a standard testing domain in Reinforcement learning). Use the left/right keys to control the car.

### Spaceinvaders

This is a simplified version of the [Space Invaders](https://en.wikipedia.org/wiki/Space_Invaders) video game. Use the left/right keys to move the laser cannon and the space key to fire (or to restart the game).

