# Turtlet 🐢

Turtlet is an interactive [Wordle](https://www.nytimes.com/games/wordle/index.html) solver.

## Installing

Install [the GHC compiler and the Cabal build system](https://www.haskell.org/downloads/). Make sure that `~/.cabal/bin` gets added to your `PATH` (the GHCup installer should ask).

Run the following command in the project root:

```sh
$ cabal install --overwrite-policy=always
```

And that's it!

Run with 

```sh
$ turtlet
```