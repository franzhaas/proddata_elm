port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (Error(..), Value, decodeValue, string, int, decodeString, field, string, Decoder, map2)
import Dict


type alias Model =
    { dataFromJS : String
    , jsonError : Maybe Error
    }

type alias Lot_pf =
    { lot : String
    , pf_case : Int
    }

type alias Results = Dict.Dict Lot_pf Int

lot_pfDecoder : Decoder Lot_pf
lot_pfDecoder =
  map2 Lot_pf
    (field "LOT" string)
    (field "pf_case" int)


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
            viewDataFromJS model.dataFromJS


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
                        lotDec = Json.Decode.decodeString (field "LOT" string) data
                    in
                        case lotDec of
                            Ok lot ->
                                  ( { model | dataFromJS = lot }, Cmd.none )
                            Err error ->
                                  (model , Cmd.none )
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
    }

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
