----------------------------------------------------------------------
--
-- HelpBoards.elm
-- Boards for the help screen
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module HelpBoards exposing (helpBoards)

import Board
import Dict
import PuzzleDB
import RenderBoard
import SharedTypes
    exposing
        ( GameState
        , HelpModelDict
        , HintsBoard
        , IntBoard
        , MaybeHelpModelDict(..)
        , Model
        , Page(..)
        , SavedModel
        )


hintsFromNestedList : List (List (List Int)) -> HintsBoard
hintsFromNestedList list =
    { rows = 3
    , cols = 3
    , defaultValue = []
    , spec = Nothing
    , index = Nothing
    , array = Board.arrayFromNestedList list
    }


makeGameState : IntBoard -> IntBoard -> HintsBoard -> GameState
makeGameState board guesses hints =
    let
        gs =
            RenderBoard.makeGameState board
    in
    { gs
        | guesses = guesses
        , hints = hints
    }


makeSavedModel : IntBoard -> IntBoard -> HintsBoard -> SavedModel
makeSavedModel board guesses hints =
    { kind = board.rows
    , index = 1
    , indices = []
    , gencount = 1
    , page = HelpPage
    , gameState = makeGameState board guesses hints
    }


helpBoard : IntBoard
helpBoard =
    PuzzleDB.boardFromSpec 3 "310143021"


helpGuesses : IntBoard
helpGuesses =
    PuzzleDB.boardFromSpec 3 "000000000"


helpHints : HintsBoard
helpHints =
    hintsFromNestedList
        [ [ [ 1, 3 ], [ 1 ], [] ]
        , [ [ 1, 3 ], [ 1, 2, 4 ], [ 1, 3 ] ]
        , [ [], [ 1, 2 ], [ 1 ] ]
        ]


tacticsGuesses2 : IntBoard
tacticsGuesses2 =
    PuzzleDB.boardFromSpec 3 "010000001"


tacticsHints2 : HintsBoard
tacticsHints2 =
    hintsFromNestedList
        [ [ [ 3 ], [], [] ]
        , [ [ 1, 3 ], [ 2, 4 ], [ 3 ] ]
        , [ [], [ 2 ], [] ]
        ]


tacticsGuesses3 : IntBoard
tacticsGuesses3 =
    PuzzleDB.boardFromSpec 3 "310003021"


tacticsGuesses4 : IntBoard
tacticsGuesses4 =
    PuzzleDB.boardFromSpec 3 "310143021"


tacticsHints3 : HintsBoard
tacticsHints3 =
    hintsFromNestedList
        [ [ [], [], [] ]
        , [ [ 1 ], [ 4 ], [] ]
        , [ [], [], [] ]
        ]


board2 : IntBoard
board2 =
    PuzzleDB.boardFromSpec 6 "290610/123450/001700/004800/012983/085071"


board2Guesses : IntBoard
board2Guesses =
    PuzzleDB.boardFromSpec 6 "000000/000000/000000/000000/000000/000000"


board2Hints1 : HintsBoard
board2Hints1 =
    hintsFromNestedList
        [ [ [ 1, 2 ], [ 9 ], [], [ 2, 3, 5, 6 ], [ 1, 2, 4, 5 ], [] ]
        , [ [ 1, 2 ], [ 2 ], [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ], [ 1, 2, 4, 5 ], [] ]
        , [ [], [], [ 1, 2, 3, 5 ], [ 3, 5, 6, 7 ], [], [] ]
        , [ [], [], [ 3, 4, 5 ], [ 7, 8, 9 ], [], [] ]
        , [ [], [ 1 ], [ 1, 2, 3, 4, 5 ], [], [ 8 ], [ 1, 3 ] ]
        , [ [], [ 8, 9 ], [ 4, 5 ], [], [ 5, 7 ], [ 1, 3 ] ]
        ]


board2Guesses2 : IntBoard
board2Guesses2 =
    PuzzleDB.boardFromSpec 6 "290000/120000/000000/000000/010083/085071"


board2Hints2 : HintsBoard
board2Hints2 =
    hintsFromNestedList
        [ [ [], [], [], [ 5, 6 ], [ 1, 2 ], [] ]
        , [ [], [], [ 3, 4 ], [ 3, 4, 5 ], [ 4, 5 ], [] ]
        , [ [], [], [ 1, 2, 3 ], [ 5, 6, 7 ], [], [] ]
        , [ [], [], [ 3, 4 ], [ 8, 9 ], [], [] ]
        , [ [], [], [ 2, 3, 4 ], [], [], [] ]
        , [ [], [], [], [], [], [] ]
        ]


board2Guesses3 : IntBoard
board2Guesses3 =
    PuzzleDB.boardFromSpec 6 "290000/120000/001700/000000/010083/085071"


board2Hints3 : HintsBoard
board2Hints3 =
    hintsFromNestedList
        [ [ [], [], [], [ 5, 6 ], [ 1, 2 ], [] ]
        , [ [], [], [ 3, 4 ], [ 3, 4, 5 ], [ 4, 5 ], [] ]
        , [ [], [], [], [], [], [] ]
        , [ [], [], [ 3, 4 ], [ 8, 9 ], [], [] ]
        , [ [], [], [ 2, 3, 4 ], [], [], [] ]
        , [ [], [], [], [], [], [] ]
        ]


board2Guesses4 : IntBoard
board2Guesses4 =
    PuzzleDB.boardFromSpec 6 "290000/120000/001700/000800/012983/085071"


board2Hints4 : HintsBoard
board2Hints4 =
    board2Hints3


board2Guesses5 : IntBoard
board2Guesses5 =
    PuzzleDB.boardFromSpec 6 "290610/123450/001700/004800/012983/085071"


board2Hints5 : HintsBoard
board2Hints5 =
    board2Hints4


makeHelpModel : IntBoard -> IntBoard -> HintsBoard -> Model
makeHelpModel board guesses hints =
    SharedTypes.savedModelToModel <|
        makeSavedModel board guesses hints


helpBoards : HelpModelDict
helpBoards =
    Dict.fromList
        [ ( "help1", makeHelpModel helpBoard helpGuesses helpHints )
        , ( "help2", makeHelpModel helpBoard helpBoard helpHints )
        , ( "tactics2", makeHelpModel helpBoard tacticsGuesses2 tacticsHints2 )
        , ( "tactics3", makeHelpModel helpBoard tacticsGuesses3 tacticsHints3 )
        , ( "tactics4", makeHelpModel helpBoard tacticsGuesses4 tacticsHints3 )
        , ( "board2Model1", makeHelpModel board2 board2Guesses board2Hints1 )
        , ( "board2Model2", makeHelpModel board2 board2Guesses2 board2Hints2 )
        , ( "board2Model3", makeHelpModel board2 board2Guesses3 board2Hints3 )
        , ( "board2Model4", makeHelpModel board2 board2Guesses4 board2Hints4 )
        , ( "board2Model5", makeHelpModel board2 board2Guesses5 board2Hints5 )
        ]
