game2048
========

[![Build Status](https://travis-ci.org/league/game2048.svg?branch=master)](https://travis-ci.org/league/game2048)

Attempt at AI for the 2048 puzzle. (I'm mainly using this as a vehicle for
learning more Haskell.) Currently it gets to 2048 some of the time, but not
consistently. The heuristic surely needs to be tuned. It's fast to search to
depth 2 (two rounds of player's move then random placement). Searching to depth
3 is plausible, but 4 will try your patience.

To build/run:

````
$ cabal build
$ dist/build/game2048/game2048 auto +RTS -N
````

If you'd like to control the game play (but still get statistics from the AI,
leave out the `auth` argument:

````
$ dist/build/game2048/game2048 +RTS -N
````

and then type U/D/L/R to move, or Q to quit.

