module App exposing (..)

import Html exposing (Html, text, div, img)
import Graphics.Render exposing (..)
import LogoForms exposing (logoForms, emptyPoint)
import MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)
import Mouse exposing (Position)
import Array exposing (..)


type alias Piece =
    { figure : Int
    , rotation : Float
    , position : Point
    , mouseReference : Maybe Position
    }


type alias Model =
    { pieces : List Piece
    , selected : Maybe Piece
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
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( initModel, initCmd )


type Msg
    = SelectForm Piece
    | LeaveForm Position
    | Move Position
    | SetPosition Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition position ->
            ( { model | selected = setPositionFix position model.selected }
            , Cmd.none
            )

        SelectForm piece ->
            ( { model | selected = Just piece }
            , Cmd.none
            )

        LeaveForm position ->
            ( { model | selected = Nothing }
            , Cmd.none
            )

        Move position ->
            ( { model | pieces = updatePieces model position }
            , Cmd.none
            )


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


renderCollage : Model -> Html Msg
renderCollage model =
    model.pieces
        |> List.map movePiece
        |> group
        |> svg 0 0 800 800


view : Model -> Html Msg
view model =
    div []
        [ renderCollage model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selected of
        Nothing ->
            Sub.batch [ Mouse.downs SetPosition ]

        Just _ ->
            Sub.batch [ Mouse.moves Move, Mouse.ups LeaveForm, Mouse.downs SetPosition ]
