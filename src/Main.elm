module Main exposing (main)

import Element exposing (Element, alignRight, centerY, el, fill, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


main =
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
                , onChange = \_ -> ()
                , placeholder = Nothing
                , label = Input.labelHidden "search text"
                }
            , el [] (text "content")
            , el [] (text "footer")
            ]
