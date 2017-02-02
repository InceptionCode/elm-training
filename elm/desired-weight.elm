module Main exposing (..)

import Html exposing (Html, beginnerProgram, div, br, h1, h2, p, text, button, input)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import String exposing (toInt)
import Basics exposing (abs)


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- Model


type alias Model =
    { weightInput : String
    , desiredWeightInput : String
    , weightCount : String
    }


model : Model
model =
    { weightInput = ""
    , desiredWeightInput = ""
    , weightCount = ""
    }



-- Update


type Msg
    = UserWeight String
    | DesiredWeight String
    | DetermineWeight


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserWeight userWeight ->
            { model | weightInput = userWeight }

        DesiredWeight userWeight ->
            { model | desiredWeightInput = userWeight }

        DetermineWeight ->
            let
                amount =
                    (testWeights model.weightInput model.desiredWeightInput)
            in
                { model | weightCount = amount }



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helper / Util Functions


testWeights : String -> String -> String
testWeights a b =
    let
        ( newA, newB ) =
            ( toInt a, toInt b )
    in
        if newA /= newB then
            returnWeightVal (Result.map2 (-) newA newB)
        else
            "You are already there!"


returnWeightVal : Result String Int -> String
returnWeightVal result =
    case result of
        Ok val ->
            abs val
                |> toString
                |> (++) "You are this far away from your desired weight: "

        Err error ->
            error



-- View


view : Model -> Html Msg
view model =
    let
        weightAmounts =
            (testWeights model.weightInput model.desiredWeightInput)
    in
        div [ class "weight-app" ]
            [ h2 [] [ text "Determine How Close You Are" ]
            , h1 [] [ text "To Your Desired Weight" ]
            , p [ class "counter" ] [ text (toString model.weightCount) ]
            , input [ type_ "text", placeholder "Add current weight", onInput UserWeight ] []
            , input [ type_ "text", placeholder "Add desired weight", onInput DesiredWeight ] []
            , button [ class "calc-button", onClick DetermineWeight ] [ text "Find Out" ]
            ]
