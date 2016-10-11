krazydad.hs is a Haskell program to snarf Kakuro board layouts from krazydad.com. This was a lot easier than generating them myself. My interest is more in game play than board generation.

To run krazydad.hs, you may need to install Network.Wreq in your Haskell. This takes a while.

    cabal install Wreq

To print the specs for 100 puzzles, starting from the easiest:

    $ ghci
    GHCi, version...
    Prelude> :l crazydad.hs
    *Main> putPuzzleSpecs 100 firstPuzzle
    (6, 1, 1, 1, ".38.31.13.12..139..798..31.92.92.21.")
    ...
    Puzzle {puzzleKind = Kind8, puzzleVolume = 7, puzzleBook = 4, puzzleNumber = 1}

To start from there to print some more:

    *Main> putPuzzleSpecs 200 (Puzzle {puzzleKind = Kind8, puzzleVolume = 7, puzzleBook = 4, puzzleNumber = 1})

But please don't cost Jim Bumgardner lots of web bandwidth, unless you also play his puzzles on his web site, and enable ads there (I don't see any, but Opera says they're there).

Thank you, Jim for creating these puzzles. I wish you'd publish your algorithm.
