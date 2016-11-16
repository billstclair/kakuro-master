import SharedTypes exposing ( GameState, SavedModel
                            , Labels, Hints, Flags, Selection
                            , IntBoard, LabelsBoard, HintsBoard
                            )
import Board exposing (Board)

import Html exposing (Html, div, text)
import Json.Decode as JD exposing (field, Decoder)
import Json.Encode as JE

type alias Model =
    Int

main =
    Html.beginnerProgram
        { model = 1
        , view = view
        , update = update
        }

update : msg -> model -> model
update msg model =
    model

br : Html msg
br =
    Html.br [][]

view : model -> Html msg
view model =
    div []
        [ text <| "hash: " ++ (toString (decodeGameState hash))
        , br
        , text <| "kakuroDojo: " ++ kakuroDojo
        ]

hash : String
hash =
    "{\"version\":7,\"board\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":\".38.31.13.12..139..798..31.92.92.21.\",\"index\":1,\"array\":[[0,3,8,0,3,1],[0,1,3,0,1,2],[0,0,1,3,9,0],[0,7,9,8,0,0],[3,1,0,9,2,0],[9,2,0,2,1,0]]},\"labels\":{\"rows\":7,\"cols\":7,\"defaultValue\":[0,0],\"spec\":null,\"index\":null,\"array\":[[[0,0],[0,0],[0,4],[0,21],[0,0],[0,13],[0,3]],[[0,0],[11,0],[0,0],[0,0],[4,0],[0,0],[0,0]],[[0,0],[4,0],[0,0],[0,0],[3,22],[0,0],[0,0]],[[0,0],[0,0],[13,10],[0,0],[0,0],[0,0],[0,0]],[[0,0],[24,12],[0,0],[0,0],[0,0],[0,3],[0,0]],[[4,0],[0,0],[0,0],[11,0],[0,0],[0,0],[0,0]],[[11,0],[0,0],[0,0],[3,0],[0,0],[0,0],[0,0]]]},\"allDone\":false,\"guesses\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":null,\"index\":null,\"array\":[[0,3,8,0,3,1],[0,1,3,0,1,2],[0,0,1,3,9,0],[0,7,9,8,0,0],[3,1,0,9,2,0],[9,2,0,2,1,0]]},\"hints\":{\"rows\":6,\"cols\":6,\"defaultValue\":[],\"spec\":null,\"index\":null,\"array\":[[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]],[[],[],[],[],[],[]]]},\"flags\":{\"isHintInput\":false,\"showPossibilities\":true},\"selection\":[2,2]}"

kakuroDojo: String
kakuroDojo =
    "{\"version\":7,\"kind\":6,\"index\":3,\"gencount\":2,\"gameState\":{\"version\":7,\"board\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":\".24.98.96185..72....84..14938.39.59.\",\"index\":3,\"array\":[[0,2,4,0,9,8],[0,9,6,1,8,5],[0,0,7,2,0,0],[0,0,8,4,0,0],[1,4,9,3,8,0],[3,9,0,5,9,0]]},\"labels\":{\"rows\":7,\"cols\":7,\"defaultValue\":[0,0],\"spec\":null,\"index\":null,\"array\":[[[0,0],[0,0],[0,11],[0,34],[0,0],[0,17],[0,13]],[[0,0],[6,0],[0,0],[0,0],[17,15],[0,0],[0,0]],[[0,0],[29,0],[0,0],[0,0],[0,0],[0,0],[0,0]],[[0,0],[0,0],[9,0],[0,0],[0,0],[0,0],[0,0]],[[0,0],[0,4],[12,13],[0,0],[0,0],[0,17],[0,0]],[[25,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]],[[12,0],[0,0],[0,0],[14,0],[0,0],[0,0],[0,0]]]},\"allDone\":false,\"guesses\":{\"rows\":6,\"cols\":6,\"defaultValue\":0,\"spec\":null,\"index\":null,\"array\":[[0,2,4,0,9,8],[0,9,6,1,8,5],[0,0,7,2,0,0],[0,0,8,4,0,0],[0,0,9,3,8,0],[0,0,0,5,9,0]]},\"hints\":{\"rows\":6,\"cols\":6,\"defaultValue\":[],\"spec\":null,\"index\":null,\"array\":[[[],[],[],[],[9,8],[]],[[],[],[],[],[9,8],[]],[[],[],[7,6,5],[4,3,2],[],[]],[[],[],[8,9],[4,3],[],[]],[[1,3],[],[9,8],[],[9,8],[]],[[1,3],[],[],[6,5],[9,8],[]]]},\"flags\":{\"isHintInput\":false,\"showPossibilities\":true},\"selection\":[4,3]},\"time\":0}"

version0 : Int
version0 =
    7

xBoardDecoder : Decoder x -> Decoder (Board x)
xBoardDecoder xDecoder =
    let xad = JD.array (JD.array xDecoder)
    in
      JD.map6
          Board.makeWithAll
          (field "rows" JD.int)
          (field "cols" JD.int)
          (field "defaultValue" xDecoder)
          (field "spec" (JD.nullable JD.string))
          (field "index" (JD.nullable JD.int))
          (field "array" xad)

intBoardDecoder : Decoder IntBoard
intBoardDecoder =
    xBoardDecoder JD.int

listToLabels : List Int -> Labels
listToLabels list =
    case list of
      [x, y] -> (x, y)
      _ -> (0, 0)

labelsDecoder : Decoder Labels
labelsDecoder =
    JD.list JD.int
        |> JD.map listToLabels

labelsBoardDecoder : Decoder LabelsBoard
labelsBoardDecoder =
    xBoardDecoder labelsDecoder

hintsDecoder : Decoder Hints
hintsDecoder =
    JD.list JD.int

hintsBoardDecoder : Decoder HintsBoard
hintsBoardDecoder =
    xBoardDecoder hintsDecoder

{-
    { version : Int --modelVersion
    , board : IntBoard
    , labels : LabelsBoard
    , allDone : Bool
    , guesses : IntBoard
    , hints : HintsBoard
    , flags : Flags
    , selection : Maybe Selection

decodeGameState0 : String -> GameState
decodeGameState json =
-}
    
flagsDecoder : Decoder Flags
flagsDecoder =
    JD.map2
        Flags
        (field "isHintInput" JD.bool)
        (field "showPossibilities" JD.bool)

listToMaybeSelection : Maybe (List Int) -> Maybe Selection
listToMaybeSelection list =
    case list of
      Just [x, y] -> Just (x, y)
      _ -> Nothing

maybeSelectionDecoder : Decoder (Maybe Selection)
maybeSelectionDecoder =
    JD.nullable (JD.list JD.int)
        |> JD.map listToMaybeSelection

gameStateDecoder : Decoder GameState
gameStateDecoder =
    JD.map8
        GameState
        (field "version" JD.int)
        (field "board" intBoardDecoder)
        (field "labels" labelsBoardDecoder)
        (field "allDone" JD.bool)
        (field "guesses" intBoardDecoder)
        (field "hints" hintsBoardDecoder)
        (field "flags" flagsDecoder)
        (field "selection" maybeSelectionDecoder)

decodeGameState : String -> Result String GameState
decodeGameState json =
    JD.decodeString gameStateDecoder json
