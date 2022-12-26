port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..), Value, decodeValue, string, int, decodeString, field, string, Decoder, map3)
import Dict
import Debug



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
    }

type alias ProdDatType = 
    {
      lot : String
    , pf_case : String
    , count : Int
    }

prodDatDecoder : Decoder ProdDatType
prodDatDecoder =
  map3 ProdDatType
    (field "LOT" string)
    (field "type" string)
    (field "count" int)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendDataToJS ]
          [ text "Send Data to JavaScript" ]
          , viewDataFromJSOrError model]


viewDataFromJSOrError : Model -> Html Msg
viewDataFromJSOrError model =
    case model.jsonError of
        Just error ->
            viewError error

        Nothing ->
            let
               oData = Debug.toString model.result
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
                                    ( { model | result = Dict.insert lpf_key prodDat.count model.result }, Cmd.none )
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
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
