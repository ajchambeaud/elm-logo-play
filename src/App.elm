module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (..)
import Graphics.Render exposing (Point, Form, angle, group, svg, position)
import LogoForms exposing (logoForms, emptyPoint)
import MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)
import Mouse exposing (Position)
import Array exposing (..)


type Action
    = Drag
    | Rotate
    | NoAction


type Msg
    = SelectForm Piece
    | LeaveForm Position
    | Move Position
    | SetPosition Position
    | SetAction Action
    | Reset


type alias Piece =
    { figure : Int
    , rotation : Float
    , position : Point
    , mouseReference : Maybe Position
    }


type alias Model =
    { pieces : List Piece
    , selected : Maybe Piece
    , action : Action
    }


positionToPoint : Position -> Point
positionToPoint position =
    ( toFloat position.x, toFloat position.y )


pointToPosition : Point -> Position
pointToPosition point =
    { x =
        point
            |> Tuple.first
            |> Basics.round
    , y =
        point
            |> Tuple.second
            |> Basics.round
    }


positionDiff : Position -> Position -> Position
positionDiff a b =
    { x = a.x - b.x
    , y = a.y - b.y
    }


initCmd : Cmd Msg
initCmd =
    Cmd.none


initModel : Model
initModel =
    { pieces =
        [ Piece 0 (degrees 180) ( 500, 500 ) Nothing
        , Piece 1 (degrees 270) ( 295, 495 ) Nothing
        , Piece 2 (degrees 90) ( 505, 405 ) Nothing
        , Piece 3 (degrees 0) ( 405, 345 ) Nothing
        , Piece 4 (degrees 0) ( 355, 345 ) Nothing
        , Piece 5 (degrees 0) ( 300, 290 ) Nothing
        , Piece 6 (degrees 90) ( 505, 290 ) Nothing
        ]
    , selected = Nothing
    , action = NoAction
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( initModel, initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.action, msg ) of
        ( _, SetPosition position ) ->
            ( { model | selected = setPositionFix position model.selected }
            , Cmd.none
            )

        ( _, SetAction action ) ->
            ( { model | action = action }
            , Cmd.none
            )

        ( Drag, SelectForm piece ) ->
            ( { model | selected = Just piece }
            , Cmd.none
            )

        ( Drag, LeaveForm position ) ->
            ( { model | selected = Nothing }
            , Cmd.none
            )

        ( Drag, Move position ) ->
            ( { model | pieces = updatePieces model position }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


setPositionFix : Position -> Maybe Piece -> Maybe Piece
setPositionFix position selected =
    case selected of
        Just piece ->
            case piece.mouseReference of
                Nothing ->
                    Just
                        { piece
                            | mouseReference =
                                piece.position
                                    |> pointToPosition
                                    |> positionDiff position
                                    |> Just
                        }

                Just p ->
                    Just piece

        Nothing ->
            Nothing


calculateNewPoint : Position -> Maybe Position -> Point -> Point
calculateNewPoint mousePosition positionFix current =
    let
        ( currentX, currentY ) =
            current
    in
        case positionFix of
            Just pfix ->
                ( (toFloat mousePosition.x - toFloat pfix.x)
                , (toFloat mousePosition.y - toFloat pfix.y)
                )

            Nothing ->
                current


updatePieces : Model -> Position -> List Piece
updatePieces model position =
    let
        list =
            model.pieces

        selected =
            model.selected
    in
        case selected of
            Just selectedPiece ->
                list
                    |> List.map
                        (\piece ->
                            if piece.figure /= selectedPiece.figure then
                                piece
                            else
                                { piece
                                    | position = calculateNewPoint position selectedPiece.mouseReference piece.position
                                    , mouseReference = selectedPiece.mouseReference
                                }
                        )

            Nothing ->
                list


movePiece : Piece -> Form Msg
movePiece piece =
    let
        form =
            logoForms
                |> get piece.figure
                |> Maybe.withDefault emptyPoint
    in
        form
            |> position piece.position
            |> angle piece.rotation
            |> onDragStart (SelectForm piece)


collageControlls : Model -> Html Msg
collageControlls model =
    div [ class "collage-controlls" ]
        [ text "Edit logo"
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-drag", True )
                , ( "active", model.action == Drag )
                ]
            , onClick (SetAction Drag)
            ]
            [ Html.text "Move pieces" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-rotate", True )
                , ( "active", model.action == Rotate )
                ]
            , onClick (SetAction Rotate)
            ]
            [ Html.text "Rotate pieces" ]
        , button
            [ class "btn btn-reset"
            , onClick Reset
            ]
            [ Html.text "Reset" ]
        ]


collage : Model -> Html Msg
collage model =
    div [ class "collage" ]
        [ if model.action == NoAction then
            (text "")
          else
            collageControlls model
        , model.pieces
            |> List.map movePiece
            |> group
            |> svg 0 0 800 800
        ]


controlls : Html Msg
controlls =
    div [ class "controlls" ]
        [ button
            [ class "btn btn-edit"
            , onClick (SetAction Drag)
            ]
            [ Html.text "edit" ]
        ]


sidebar : Html Msg
sidebar =
    div [ class "sidebar" ]
        [ h1 []
            [ Html.text "elm-logo.play" ]
        , controlls
        ]


view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ sidebar
        , collage model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.selected, model.action ) of
        ( Nothing, _ ) ->
            Sub.batch [ Mouse.downs SetPosition ]

        ( Just _, Drag ) ->
            Sub.batch [ Mouse.moves Move, Mouse.ups LeaveForm, Mouse.downs SetPosition ]

        _ ->
            Sub.none
