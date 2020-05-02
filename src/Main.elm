module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http


server_url =
    "http://localhost:8000"


type alias Model =
    { searchText : String
    , bodyText : String
    }


type Msg
    = WebResponse (Result Http.Error String)
    | WebRequestButtonPressed
    | SearchFieldUpdated String


init : () -> ( Model, Cmd Msg )
init flags =
    ( { searchText = "", bodyText = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchFieldUpdated s ->
            ( { model | searchText = s }, Cmd.none )

        WebRequestButtonPressed ->
            ( model
            , Http.get
                { url = "http://neverssl.com/"
                , expect = Http.expectString WebResponse
                }
            )

        WebResponse (Ok s) ->
            ( { model | bodyText = s }, Cmd.none )

        WebResponse (Err e) ->
            ( { model | bodyText = "Got an error" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgb255 32 32 32)
        , Font.color (rgb255 220 220 220)
        , padding 20
        ]
    <|
        Element.column
            [ width fill
            , Element.centerX
            , Background.color (rgb255 32 32 32)
            , spacing 5
            ]
            [ Input.text
                [ Border.rounded 10000
                , Border.solid
                , Border.color (rgb255 75 75 75)
                , Border.width 1
                , Element.paddingEach
                    { top = 10
                    , bottom = 10
                    , left = 20
                    , right = 10
                    }
                , width fill
                , Background.color (rgb255 20 20 20)
                ]
                { text = "search"
                , onChange = \s -> SearchFieldUpdated s
                , placeholder = Nothing
                , label = Input.labelHidden "search text "
                }
            , el [] (text model.bodyText)
            , el [] (text "content")
            , el [] (text "footer")
            , Input.button
                []
                { onPress = Just WebRequestButtonPressed
                , label = Element.text "Test HTTP"
                }
            ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
