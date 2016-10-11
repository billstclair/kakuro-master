krazydad.hs is a Haskell program to snarf some board layouts from krazydad.com. This was a lot easier than generating them myself. My interest is more in game play than board generation.

To run krazydad.hs, you may need to install Network.Wreq in your Haskell. This takes a while.

    cabal install Wreq

Then, to extract puzzles to file:

    runhaskell crazydad.hs
    
Or to print the extraction for one file, in ghci:

    :l crazydad.hs
    getPuzzleSpec $ Puzzle 6 1 1 1
