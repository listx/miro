# [Miro][]

[![Build Status](https://travis-ci.org/listx/miro.svg?branch=master)](https://travis-ci.org/listx/miro)

Miro is a maze generator that outputs to ASCII/Unicode. It is inspired by the
book *Mazes for Programmers* (2015) by Jamis Buck. The name *miro* comes from
the Sino-Korean *미로* (迷路), which means *maze*.

# Install

Miro uses [stack][]. After installing Stack, run `stack build` in the root of
this repo.

## Haddock

Run `stack haddock`.

# Examples

```
# Ask for help.
stack exec -- miro --help
# Generate a basic Sidewinder maze.
stack exec -- miro --maze-type sidewinder --output ascii --rng-seed '(0,0)' --size '(19,19)' --quiet
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
|                                                                           |
+---+---+   +---+   +---+   +   +   +---+---+---+   +   +   +---+   +---+   +
|               |       |   |   |               |   |   |       |       |   |
+   +---+   +   +   +---+   +---+---+   +   +---+---+   +---+---+   +---+   +
|   |       |   |       |           |   |   |           |           |       |
+---+   +---+   +---+---+---+   +---+---+   +---+   +   +   +---+   +   +   +
|       |       |                   |           |   |   |   |       |   |   |
+---+   +---+   +   +   +---+---+   +---+   +   +---+   +   +   +   +---+---+
|           |   |   |   |               |   |       |   |   |   |           |
+---+   +---+   +   +   +   +   +---+---+---+---+   +   +---+---+   +   +   +
|       |       |   |   |   |                   |   |   |           |   |   |
+   +   +---+   +   +   +---+---+   +---+   +---+   +   +---+   +---+---+   +
|   |       |   |   |           |   |           |   |       |       |       |
+   +   +   +---+---+---+---+---+---+   +---+---+---+---+---+---+---+---+   +
|   |   |           |                                           |           |
+   +   +---+---+---+   +---+---+   +---+---+---+   +---+---+---+---+   +---+
|   |   |               |                   |           |                   |
+   +   +---+   +   +---+---+---+---+---+---+   +---+---+   +---+---+   +---+
|   |       |   |                       |       |           |               |
+---+---+   +---+---+   +   +---+   +   +---+---+---+   +---+   +---+---+---+
|                   |   |   |       |           |       |                   |
+   +---+---+   +---+---+   +   +---+---+   +   +---+   +   +   +---+---+   +
|   |           |           |   |           |       |   |   |       |       |
+---+   +---+   +---+   +---+   +   +   +   +   +---+---+   +   +   +---+   +
|       |           |       |   |   |   |   |   |           |   |       |   |
+   +---+---+   +   +---+---+---+   +---+---+---+   +---+   +   +---+   +   +
|           |   |   |                           |       |   |       |   |   |
+   +   +---+---+---+   +   +   +---+---+---+   +   +---+   +   +---+---+   +
|   |           |       |   |               |   |   |       |       |       |
+   +---+   +   +   +---+---+   +   +---+   +   +---+   +   +---+---+   +   +
|   |       |   |       |       |       |   |       |   |   |           |   |
+   +   +---+   +   +---+---+   +---+---+   +   +   +---+---+   +---+   +   +
|   |       |   |           |       |       |   |           |       |   |   |
+   +   +---+---+---+---+   +---+---+---+   +   +---+---+---+   +   +---+   +
|   |   |                       |           |           |       |   |       |
+   +---+   +   +   +   +---+   +   +---+---+   +---+---+   +   +---+   +   +
|       |   |   |   |       |   |   |               |       |       |   |   |
+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
```

# Plans

Ultimately, the goal of this project is to add all of the algorithms featured in
Buck's book. In part this means porting Buck's Ruby code to Haskell.

# Caveats

I will politely decline pull requests that deal with adding new algorithms
because I want to do that part myself as a learning exercise. But if you have
fixes for obvious mistakes, please let me know!

[miro]: https://github.com/listx/miro
[stack]: https://docs.haskellstack.org
