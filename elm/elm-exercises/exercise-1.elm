module Main exposing (..)

import Html exposing (Html, br, div, text, h2)
import String exposing (..)
import List exposing (length)


-- Simple Function Exercise


printNameWLength : String -> String
printNameWLength name =
    if (String.length name) >= 10 then
        toString (String.length name)
            |> String.append (toUpper name ++ " - name length:")
    else
        String.append name " Name length is to short"



-- More Function Exercises


(~=) : String -> String -> Bool
(~=) a b =
    String.left 1 a == String.left 1 b


wordCount : String -> String
wordCount string =
    (String.words >> List.length >> toString) string


main : Html msg
main =
    div []
        [ h2 [] [ text "Print Name and Length" ]
        , text (printNameWLength "Darrell Washington")
        , br [] []
        , h2 [] [ text "Is the first letter the same" ]
        , text (toString ("Darrell" ~= "David"))
        , br [] []
        , text (toString ((~=) "Darrell" "karnell"))
        , h2 [] [ text "What is the word count?" ]
        , text ((wordCount "David Lee is the best"))
        ]
