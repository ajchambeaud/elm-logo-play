module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (..)
import Graphics.Render exposing (Point, Form, angle, group, svg, position, opacity)
import LogoForms exposing (logoForms, emptyPoint)
import MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)
import Keyboard exposing (KeyCode)
import Key exposing (..)
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
    | KeyPress KeyCode
    | Reset
    | Cancel
    | Save


type alias Piece =
    { figure : Int
    , rotation : Float
    , position : Point
    , mouseReference : Maybe Position
    , selected : Bool
    }


type alias Model =
    { pieces : List Piece
    , action : Action
    , active : Bool
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
        [ Piece 0 (degrees 180) ( 500, 500 ) Nothing False
        , Piece 1 (degrees 270) ( 295, 495 ) Nothing False
        , Piece 2 (degrees 90) ( 505, 405 ) Nothing False
        , Piece 3 (degrees 0) ( 405, 345 ) Nothing False
        , Piece 4 (degrees 0) ( 355, 345 ) Nothing False
        , Piece 5 (degrees 0) ( 300, 290 ) Nothing False
        , Piece 6 (degrees 90) ( 505, 290 ) Nothing False
        ]
    , action = NoAction
    , active = False
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( initModel, initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.action, msg ) of
        ( _, SetPosition position ) ->
            ( { model | pieces = setPositionFix position model.pieces }
            , Cmd.none
            )

        ( _, SetAction action ) ->
            ( { model
                | action = action
                , pieces = unselect model.pieces
              }
            , Cmd.none
            )

        ( Drag, SelectForm piece ) ->
            ( { model
                | pieces = selectPiece piece model.pieces
                , active = True
              }
            , Cmd.none
            )

        ( Rotate, SelectForm piece ) ->
            ( { model
                | pieces = selectPiece piece model.pieces
                , active = True
              }
            , Cmd.none
            )

        ( Drag, LeaveForm position ) ->
            ( { model
                | pieces = unselect model.pieces
                , active = False
              }
            , Cmd.none
            )

        ( Drag, Move position ) ->
            ( { model | pieces = updatePieces model position }
            , Cmd.none
            )

        ( Rotate, KeyPress keyCode ) ->
            ( { model | pieces = rotatePieces model keyCode }
            , Cmd.none
            )

        ( _, Reset ) ->
            ( { model | pieces = initModel.pieces }
            , Cmd.none
            )

        ( _, Cancel ) ->
            ( { model
                | pieces = initModel.pieces
                , action = NoAction
              }
            , Cmd.none
            )

        ( _, Save ) ->
            ( { model
                | action = NoAction
                , pieces = movePiecesToCenter (distanceToCenter model.pieces) model.pieces
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


setPositionFix : Position -> List Piece -> List Piece
setPositionFix position pieces =
    pieces
        |> List.map
            (\piece ->
                if piece.selected then
                    { piece
                        | mouseReference =
                            piece.position
                                |> pointToPosition
                                |> positionDiff position
                                |> Just
                    }
                else
                    piece
            )


addPoint : Point -> Point -> Point
addPoint p1 p2 =
    ( Tuple.first p1 + Tuple.first p2
    , Tuple.second p1 + Tuple.second p2
    )


diffPoint : Point -> Point -> Point
diffPoint p1 p2 =
    ( Tuple.first p1 - Tuple.first p2
    , Tuple.second p1 - Tuple.second p2
    )


scalePoint : Float -> Point -> Point
scalePoint num p =
    ( Tuple.first p * num
    , Tuple.second p * num
    )


findCenter : List Piece -> Point
findCenter list =
    list
        |> List.map .position
        |> List.foldl addPoint ( 0, 0 )
        |> scalePoint (1 / 7)


distanceToCenter : List Piece -> Point
distanceToCenter =
    findCenter >> (diffPoint ( 400, 400 ))


selectPiece : Piece -> List Piece -> List Piece
selectPiece pieceSelected list =
    list
        |> List.map
            (\piece ->
                if pieceSelected.figure == piece.figure then
                    { piece | selected = True }
                else
                    { piece | selected = False }
            )


unselect : List Piece -> List Piece
unselect list =
    list
        |> List.map
            (\piece -> { piece | selected = False })


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
    model.pieces
        |> List.map
            (\piece ->
                if not piece.selected then
                    piece
                else
                    { piece
                        | position = calculateNewPoint position piece.mouseReference piece.position
                    }
            )


rotatePieces : Model -> KeyCode -> List Piece
rotatePieces model keyCode =
    let
        list =
            model.pieces

        rotate =
            case Key.fromCode keyCode of
                ArrowLeft ->
                    -(degrees 15)

                ArrowRight ->
                    degrees 15

                _ ->
                    degrees 0
    in
        list
            |> List.map
                (\piece ->
                    if not piece.selected then
                        piece
                    else
                        { piece
                            | rotation = piece.rotation + rotate
                        }
                )


movePiecesToCenter : Point -> List Piece -> List Piece
movePiecesToCenter distance pieces =
    let
        newPoint position =
            ( Tuple.first position + Tuple.first distance
            , Tuple.second position + Tuple.second distance
            )
    in
        pieces
            |> List.map (\piece -> { piece | position = newPoint piece.position })


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


formID : Form Msg -> Form Msg
formID form =
    form


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
            |> List.map moveForm
            |> group
            |> svg 0 0 800 800
        ]


controlls : Model -> Html Msg
controlls model =
    div [ class "controlls" ]
        [ if model.action == NoAction then
            button
                [ class "btn btn-edit"
                , onClick (SetAction Drag)
                ]
                [ Html.text "edit" ]
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


sidebar : Model -> Html Msg
sidebar model =
    div [ class "sidebar" ]
        [ h1 []
            [ Html.text "elm-logo.play" ]
        , controlls model
        ]


view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ sidebar model
        , collage model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.active, model.action ) of
        ( False, Drag ) ->
            Sub.batch [ Mouse.downs SetPosition ]

        ( True, Drag ) ->
            Sub.batch
                [ Mouse.moves Move
                , Mouse.ups LeaveForm
                , Mouse.downs SetPosition
                ]

        ( False, Rotate ) ->
            Sub.none

        ( True, Rotate ) ->
            Sub.batch
                [ Keyboard.downs KeyPress
                ]

        _ ->
            Sub.none
