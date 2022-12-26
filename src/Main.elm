port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..), Value, decodeValue, string, int, decodeString, field, string, Decoder, map3)
import Dict
import Debug
import List
import Round
import String



type alias Lot_pf =
    { lot : String
    , pf_case : String
    }

compare : Lot_pf -> Lot_pf -> Order
compare d1 d2 = if (d1.lot ++ d1.pf_case) > (d2.lot ++ d2.pf_case) then GT else if (d1.lot ++ d1.pf_case) == (d2.lot ++ d2.pf_case) then EQ else LT 
                    

type alias ProdDataResults = Dict.Dict (String, String) Int

type alias Model =
    { dataFromJS : String
    , jsonError : Maybe Error
    , result : ProdDataResults
    , preparedData : PreperedDataType
    }

type alias ProdDatType = 
    {
      lot : String
    , pf_case : String
    , count : Int
    }

type alias PreperedDataType =     Dict.Dict String (Int, Int, Float) 

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
            

prodDatDecoder : Decoder ProdDatType
prodDatDecoder =
  map3 ProdDatType
    (field "LOT" string)
    (field "type" string)
    (field "count" int)

-- VIEW
type alias MyListItem =
    { label : String
    , price : Float
    }

total : Float
total =
    5.0

myList : List MyListItem
myList =
    [ { label = "labelA", price = 2 }
    , { label = "labelB", price = 3 }
    ]

toTableRow : ( String, ( Int, Int, Float ) ) -> Html Msg
toTableRow myListItem =
    let
       (lot, (passes1, fails1, yield1)) = myListItem
    in
       let
          passes = String.fromInt  passes1
          fails = String.fromInt  fails1
          yield = Round.round 2  yield1
        in  
          tr []
             [ td [] [ text lot ]
             , td [] [ text passes ]
             , td [] [ text fails ]
             , td [] [ text yield ]
             ]

view : Model -> Html Msg
view model =
    let
        preparedList = Dict.toList    model.preparedData
    in
        table
            []
            ([ thead []
                [ th [] [ text "LOT" ]
                , th [] [ text "PASSES" ]
                , th [] [ text "FAILS" ]
                , th [] [ text "YIELD" ]
                ]
            ]
                ++ List.map toTableRow preparedList
            )
viewDataFromJSOrError : Model -> Html Msg
viewDataFromJSOrError model =
    case model.jsonError of
        Just error ->
            viewError error

        Nothing ->
            let
               oData = Debug.toString model.preparedData
            in
               viewDataFromJS oData


viewError : Error -> Html Msg
viewError jsonError =
    let
        errorHeading =
            "Couldn't receive data from JavaScript"

        errorMessage =
            case jsonError of
                Failure message _ ->
                    message

                _ ->
                    "Error: Invalid JSON"
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


viewDataFromJS : String -> Html msg
viewDataFromJS data =
    div []
        [ br [] []
        , strong [] [ text "Data received from JavaScript: " ]
        , text data
        ]


type Msg
    = SendDataToJS
    | ReceivedDataFromJS Value

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, Cmd.none )

        ReceivedDataFromJS value ->
            case decodeValue string value of
                Ok data ->
                    let 
                        lpf = decodeString prodDatDecoder data
                    in
                        case lpf of
                            Ok prodDat ->
                                  let
                                    lpf_key = (prodDat.lot, prodDat.pf_case)
                                  in
                                    let 
                                       nextResult = Dict.insert lpf_key prodDat.count model.result
                                    in
                                        let
                                                  nextPD = semiPreparedData model.preparedData nextResult prodDat.lot prodDat.pf_case prodDat.count
                                        in
                                              ( { model | result = nextResult, preparedData = nextPD }, Cmd.none )
                            Err error ->
                                  ( { model | dataFromJS = "Error parsing database content: " ++ data}, Cmd.none )
                Err error ->
                    ( { model | jsonError = Just error }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS

port receiveData : (Value -> msg) -> Sub msg

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
    { dataFromJS = ""
    , jsonError = Nothing
    , result = Dict.empty 
    , preparedData = Dict.empty
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
