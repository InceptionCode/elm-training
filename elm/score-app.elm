module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)


-- This app is part of the Elm for Beginners course.
-- If you would like to learn more check out the README.md file.
-- This app's design and structure will be nearly an exact replication of the app showcased on the course.
-- Model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    }



-- Update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input player ->
            { model | playerName = player }

        Cancel ->
            { model | playerName = "", playerId = Nothing }

        Save ->
            if (String.isEmpty model.playernumberthen
                model
            else
                save model

        Score player points ->
            score model player points

        Edit player ->
            { model | playerName = player.name, playerId = Just player.id }

        DeletePlay play ->
            removePlay play model


removePlay : Play -> Model -> Model
removePlay play model =
    let
        newPlays =
            List.filter (filterPlay play.id) model.plays

        newPlayers =
            List.map (subtractPoints play) model.players
    in
        { model | plays = newPlays, players = newPlayers }


filterPlay : Int -> Play -> Bool
filterPlay id play =
    play.id /= id


subtractPoints : Play -> Player -> Player
subtractPoints play players =
    if players.id == play.playerId then
        { players | points = players.points - 1 * play.points }
    else
        players


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map (checkPlayerPoints scorer.id points) model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


checkPlayer : Model -> Int -> Player -> Player
checkPlayer model id player =
    if player.id == id then
        { player | name = model.playerName }
    else
        player


checkPlay : Model -> Int -> Play -> Play
checkPlay model id play =
    if play.playerId == id then
        { play | name = model.playerName }
    else
        play


checkPlayerPoints : Int -> Int -> Player -> Player
checkPlayerPoints id points player =
    if player.id == id then
        { player | points = player.points + points }
    else
        player


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map (checkPlayer model id) model.players

        newPlays =
            List.map (checkPlay model id) model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , playerName = ""
            , playerId = Nothing
        }


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.playerName 0

        newPlayers =
            player :: model.players
    in
        { model
            | players = newPlayers
            , playerName = ""
        }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (players model.playerId)
        |> ul [ class "player-list" ]


players : Maybe Int -> Player -> Html Msg
players editPlayerId player =
    li []
        [ i [ class "fa fa-pencil-square-o", onClick (Edit player) ] []
        , div [ class (insertEditClass editPlayerId player) ] [ text player.name ]
        , button [ type_ "button", onClick (Score player 2) ] [ text "2pts" ]
        , button [ type_ "button", onClick (Score player 3) ] [ text "3pts" ]
        , div [] [ text (toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total: " ]
            , div [] [ text (toString total) ]
            ]


playerSection : Model -> Html Msg
playerSection model =
    div [ class "player-section" ]
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , class (inputEditClass model.playerId)
            , placeholder "Add/Edit Player . . ."
            , onInput Input
            , value model.playerName
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


insertEditClass : Maybe Int -> Player -> String
insertEditClass editPlayerId player =
    let
        mPlayerId =
            case editPlayerId of
                Just id ->
                    id

                Nothing ->
                    -1
    in
        if player.id == mPlayerId then
            "editing"
        else
            "not-editing"


inputEditClass : Maybe Int -> String
inputEditClass editPlayerId =
    case editPlayerId of
        Just id ->
            "editing"

        Nothing ->
            "not-editing"


playSection : Model -> Html Msg
playSection model =
    div [ class "play-section" ]
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> ul [ class "play-list" ]


play : Play -> Html Msg
play play =
    li []
        [ i [ class "fa fa-times", onClick (DeletePlay play) ] []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
