port module Main exposing (main)
import Browser
import Html exposing (Html, text, td, tr, th, thead, table)
import Json.Decode exposing (Error, Value, decodeValue, string, int, field, Decoder, map3, errorToString )
import Dict
import List
import Round
import String
                    
type alias ProdDataResults  = Dict.Dict (String, String) Int
type alias Model            = {jsError : Maybe Error, result : ProdDataResults, preparedData : PreperedDataType}
type alias ProdDatType      = {lot : String, pf_case : String, count : Int}
type alias PreperedDataType = Dict.Dict String (Int, Int, Float) 

prodDatDecoder : Decoder ProdDatType
prodDatDecoder =
  map3 ProdDatType
    (field "LOT" string)
    (field "type" string)
    (field "count" int)

semiPreparedData : PreperedDataType -> ProdDataResults -> String -> String ->  Int -> PreperedDataType
semiPreparedData prevPreparedData storage lot pf_case count =
    let
        fails  = if pf_case == "fails" then Just count else Dict.get  (lot, "fails") storage
        passes = if pf_case == "passes" then Just count else Dict.get  (lot, "passes") storage
    in
         case fails of
            Just jFails ->
                case passes of
                    Just jPasses -> 
                        Dict.insert  lot  (jPasses, jFails, (100.0*(toFloat jPasses)/(toFloat (jPasses+jFails)))) prevPreparedData
                    Nothing ->
                        Dict.insert  lot  (0, jFails, 0.0) prevPreparedData
            Nothing ->
                case passes of
                    Just jPasses ->
                        Dict.insert  lot  (jPasses, 0, 100.0) prevPreparedData
                    Nothing ->
                        prevPreparedData

toTableRow : ( String, ( Int, Int, Float ) ) -> Html Msg
toTableRow myListItem =
    let
       (lot, (passes1, fails1, yield1)) = myListItem
       passes = String.fromInt  passes1
       fails = String.fromInt  fails1
       yield = Round.round 2  yield1
    in
       tr [] [td [] [ text lot ], td [] [ text passes ] , td [] [ text fails ] , td [] [ text yield ]]

view : Model -> Html Msg
view model =
    let
        preparedList = Dict.toList    model.preparedData
    in
        case model.jsError of
            Just error  ->
                text ("JS error: " ++ (errorToString  error))
            Nothing ->
                table []
                        (thead []
                         [th [] [text "LOT" ], th [] [text "PASSES" ], th [] [text "FAILS" ], th [] [text "YIELD" ]]
                         :: List.map toTableRow preparedList)

type Msg
    = ReceivedDataFromJS Value

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDataFromJS value ->
            case decodeValue prodDatDecoder value of
                Ok prodDat ->
                    let 
                        pf_tuple = (prodDat.lot, prodDat.pf_case)
                        nextResult = Dict.insert pf_tuple prodDat.count model.result
                        nextPD = semiPreparedData model.preparedData nextResult prodDat.lot prodDat.pf_case prodDat.count
                    in
                        ({ model | result = nextResult, preparedData = nextPD }, Cmd.none)
                Err error ->
                        ({ model | jsError  = Just error}, Cmd.none)
 
subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS

port receiveData : (Value -> msg) -> Sub msg

init : () -> ( Model, Cmd Msg )
init _ =
    ({jsError = Nothing, result = Dict.empty, preparedData = Dict.empty}, Cmd.none )    

main : Program () Model Msg
main =
    Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}
