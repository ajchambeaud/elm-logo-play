module App.View exposing (view)

import App.Types exposing (..)
import App.Utils.LogoForms exposing (logoForms, emptyPoint)
import App.Utils.MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, attribute, src)
import Html.Events exposing (..)
import Graphics.Render exposing (Point, Form, angle, group, svg, position, opacity)
import Array exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Button as Button


moveForm : Piece -> Form Msg
moveForm piece =
    let
        form =
            logoForms
                |> get piece.figure
                |> Maybe.withDefault emptyPoint
    in
        form
            |> position piece.position
            |> angle piece.rotation
            |> opacity
                (if piece.selected then
                    0.5
                 else
                    1
                )
            |> onDragStart (SelectForm piece)


controllsContainer : Model -> Html Msg
controllsContainer model =
    div [ class "controlls-container" ]
        [ mainControlls model
        , if model.action == NoAction then
            (text "")
          else
            actionControlls model
        , userMenu model
        ]


getLogoAutor : Model -> String
getLogoAutor model =
    let
        logo =
            model.elmLogo
    in
        if not model.animate && logo.user /= "" then
            "Made by " ++ logo.user
        else
            ""


help : Model -> Html Msg
help model =
    let
        helpText =
            case model.action of
                NoAction ->
                    getLogoAutor model

                Drag ->
                    "Move Piece: use drag and drop to move the pieces"

                Rotate ->
                    "Rotate pieces: click on the piece and use keys ⬅️ and ➡️ to rotate it"
    in
        div [ class "help-container" ]
            [ text helpText ]


mainControlls : Model -> Html Msg
mainControlls model =
    div [ class "main-controlls" ]
        [ if model.action == NoAction then
            button
                [ class "btn btn-edit"
                , onClick Edit
                ]
                [ Html.text "Create or edit your logo" ]
          else
            text ""
        , if model.action /= NoAction then
            button
                [ class "btn btn-save"
                , onClick Save
                ]
                [ Html.text "save" ]
          else
            text ""
        , if model.action /= NoAction then
            button
                [ class "btn btn-cancel"
                , onClick Cancel
                ]
                [ Html.text "cancel" ]
          else
            text ""
        ]


actionControlls : Model -> Html Msg
actionControlls model =
    div [ class "action-controlls" ]
        [ button
            [ classList
                [ ( "btn", True )
                , ( "action-btn", True )
                , ( "active", model.action == Drag )
                ]
            , onClick (SetAction Drag)
            ]
            [ Html.text "Move piece" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "action-btn", True )
                , ( "active", model.action == Rotate )
                ]
            , onClick (SetAction Rotate)
            ]
            [ Html.text "Rotate piece" ]
        , button
            [ class "btn action-btn"
            , onClick Reset
            ]
            [ Html.text "Reset" ]
        ]


userMenu : Model -> Html Msg
userMenu model =
    div [ class "user-logout" ]
        [ button
            [ class "btn btn-logout"
            , onClick Logout
            ]
            [ Html.text "logout" ]
        ]


collage : Model -> Grid.Column Msg
collage model =
    Grid.col
        [ Col.xs9, Col.attrs [ class "collage" ] ]
        [ case model.user of
            Nothing ->
                text ""

            Just user ->
                controllsContainer model
        , model.elmLogo.logo
            |> List.map moveForm
            |> group
            |> svg 0 0 800 800
        , help model
        ]


forkMeRibbon : Html Msg
forkMeRibbon =
    a [ href "https://github.com/ajchambeaud/elm-logo-play" ]
        [ img [ attribute "data-canonical-src" "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png", src "https://camo.githubusercontent.com/8b6b8ccc6da3aa5722903da7b58eb5ab1081adee/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f6c6566745f6f72616e67655f6666373630302e706e67", attribute "style" "position: absolute; top: 0; left: 0; border: 0;" ]
            []
        ]


credits : Html Msg
credits =
    div [ class "credits" ]
        [ Html.text "Made with 🌳 by "
        , a [ class "twitter", href "https://twitter.com/ajchambeaud" ] [ Html.text "@ajchambeaud" ]
        ]


sidebar : Model -> Grid.Column Msg
sidebar model =
    Grid.col
        [ Col.xs3, Col.attrs [ class "sidebar" ] ]
        [ h1 []
            [ Html.text "elm-tangram" ]
        , case model.user of
            Just user ->
                div [ class "user-name" ] [ text user.name ]

            Nothing ->
                button
                    [ class "btn btn-login"
                    , onClick Login
                    ]
                    [ span [ class "fa fa-twitter" ] []
                    , text "Login with Twitter"
                    ]
        , credits
        , forkMeRibbon
        ]


topBar : Model -> Html Msg
topBar model =
    case model.loginError of
        Just err ->
            let
                _ =
                    Debug.log err
            in
                Grid.row []
                    [ Grid.col
                        [ Col.xs12, Col.attrs [ class "topbar" ] ]
                        [ text "There was an error getting the user information. Try again latter or "
                        , a [ href "https://github.com/ajchambeaud/elm-logo-play/issues/new" ] [ text "create an issue" ]
                        , text " \x1F913."
                        , Button.button
                            [ Button.small
                            , Button.danger
                            , Button.attrs
                                [ class "ml-1 close-topbar"
                                , onClick CloseAlert
                                ]
                            ]
                            [ text "x" ]
                        ]
                    ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    Grid.containerFluid []
        [ topBar model
        , Grid.row []
            [ sidebar model
            , collage model
            ]
        ]
