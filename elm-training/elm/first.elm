module Main exposing (..)

import Html exposing (Html, div, input, form, h1, h2, p, br, button, text)
import Html.Attributes exposing (class, placeholder, type_, style)
import Html.Events exposing (onClick, onInput)
import String exposing (reverse, length, all)
import Char exposing (isDigit)
import Random exposing (generate, int)


--The following will be a small project showing off the basics of elm.
-- This section will prodominantly follow the "Get Started" guide on elm-lang.org


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- Model


type alias Model =
    { clickAmount : Int
    , userInput : String
    , name : String
    , password : String
    , confirmPassword : String
    , confirmAge : String
    , verify : String
    , dieFace : Int
    }


initModel : Model
initModel =
    { clickAmount = 0
    , userInput = ""
    , name = ""
    , password = ""
    , confirmPassword = ""
    , confirmAge = ""
    , verify = "No"
    , dieFace = 1
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )



--subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Exercises :
-- Check that the password is longer than 8 characters.
-- Make sure the password contains upper case, lower case, and numeric characters.
-- Add an additional field for age and check that it is a number.
-- Add a "Submit" button. Only show errors after it has been pressed.
-- Functions and Extensions


buttonDisplay : Model -> Html Msg
buttonDisplay model =
    div [ class "button-section" ]
        [ h1 [] [ text "Elm-Button" ]
        , h2 [] [ text (toString model.clickAmount) ]
        , div [ class "buttons" ]
            [ button [ class "Increment", onClick Increment ] [ text "Increment" ]
            , button [ class "Decrement", onClick Decrement ] [ text "Decrement" ]
            , button [ class "reset", onClick Reset ] [ text "Reset Counter" ]
            ]
        ]


trickyInput : Model -> Html Msg
trickyInput model =
    div [ class "input-section" ]
        [ h1 [] [ text "Elm-Input Tricks" ]
        , input [ placeholder "It's going to be reversed", onInput Reverse ] []
        , p [] [ text model.userInput ]
        ]


formDisplay : Html Msg
formDisplay =
    div [ class "form-section" ]
        [ h1 [] [ text "Elm-Form" ]
        , form []
            [ input [ type_ "text", placeholder "Name", onInput Name ] []
            , input [ type_ "password", placeholder "Password", onInput Password ] []
            , input [ type_ "password", placeholder "Confirm Password", onInput ConfirmPassword ] []
            , input [ type_ "text", placeholder "Provide your Age", onInput ConfirmAge ] []
            ]
        , button [ onClick Verify ] [ text "Submit" ]
        ]



--Happy path
-- check the passwords length > then check if the passwords match. (continue)
-- check if age is a Int (continue)
-- show validation message.
--Sad path
-- password issue > show validation message.
-- Age issue > show validation message.


passwordValidation : Model -> Maybe Model
passwordValidation model =
    if length model.password >= 8 && model.password == model.confirmPassword then
        Maybe.Just model
    else
        Maybe.Nothing


type alias Styles =
    { color : String, message : String, display : String }


ageValidation : Maybe Model -> Styles
ageValidation model =
    let
        styles =
            { color = "", message = "", display = "" }
    in
        case model of
            Maybe.Nothing ->
                { styles
                    | color = "red"
                    , message = "Password error....Passwords must match and contain at least 8 characters."
                    , display = "block"
                }

            Maybe.Just model ->
                if all isDigit model.confirmAge == False then
                    { styles
                        | color = "red"
                        , message = "That's not a valid age. Age must be a number."
                        , display = "block"
                    }
                else
                    { styles
                        | color = "limegreen"
                        , message = "You are clear to sign up."
                        , display = "block"
                    }


showValidationMessage : Model -> Styles -> Html msg
showValidationMessage model { color, message, display } =
    if model.verify == "Yes" then
        p
            [ class "verification-message"
            , style [ ( "color", color ), ( "display", display ) ]
            ]
            [ text message ]
    else
        p
            [ class "verification-message"
            , style [ ( "color", color ), ( "display", "none" ) ]
            ]
            [ text message ]


formVerification : Model -> Html msg
formVerification model =
    (passwordValidation >> ageValidation) model
        |> showValidationMessage model


diceRoller : Model -> Html Msg
diceRoller model =
    div [ class "dice-section" ]
        [ h1 [] [ text "Watch Elm Roll the Dice" ]
        , h2 [] [ text (toString model.dieFace) ]
        , button [ class "roll-die", onClick Roll ] [ text "Roll" ]
        ]



-- Update as an exercise include an "reset" feature.


type Msg
    = Increment
    | Decrement
    | Reset
    | Reverse String
    | Name String
    | Password String
    | ConfirmPassword String
    | ConfirmAge String
    | Verify
    | Roll
    | NewFace Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | clickAmount = model.clickAmount + 1 }, Cmd.none )

        Decrement ->
            ( { model | clickAmount = model.clickAmount - 1 }, Cmd.none )

        Reset ->
            ( { model | clickAmount = 0 }, Cmd.none )

        Reverse input ->
            ( { model | userInput = reverse input }, Cmd.none )

        Name newName ->
            ( { model | name = newName }, Cmd.none )

        Password newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        ConfirmPassword confirmPassword ->
            ( { model | confirmPassword = confirmPassword }, Cmd.none )

        ConfirmAge age ->
            ( { model | confirmAge = age }, Cmd.none )

        Verify ->
            ( { model | verify = "Yes" }, Cmd.none )

        Roll ->
            ( model, generate NewFace (int 1 6) )

        NewFace newFace ->
            ( { model | dieFace = newFace }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ buttonDisplay model
        , trickyInput model
        , formDisplay
        , formVerification model
        , diceRoller model
        ]
