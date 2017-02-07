module App.Utils.Editor exposing (..)

import App.Types exposing (..)
import Graphics.Render exposing (Point, Form, angle, group, svg, position, opacity)
import Mouse exposing (Position)
import Keyboard exposing (KeyCode)
import App.Utils.Key as Key exposing (..)


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


setPositionFix : Position -> Logo -> Logo
setPositionFix position elmLogo =
    elmLogo
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


findCenter : Logo -> Point
findCenter list =
    list
        |> List.map .position
        |> List.foldl addPoint ( 0, 0 )
        |> scalePoint (1 / 7)


distanceToCenter : Logo -> Point
distanceToCenter =
    findCenter >> (diffPoint ( 400, 400 ))


selectPiece : Piece -> Logo -> Logo
selectPiece pieceSelected list =
    list
        |> List.map
            (\piece ->
                if pieceSelected.figure == piece.figure then
                    { piece | selected = True }
                else
                    { piece | selected = False }
            )


unselect : Logo -> Logo
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


updatePieces : Model -> Position -> Logo
updatePieces model position =
    model.elmLogo
        |> List.map
            (\piece ->
                if not piece.selected then
                    piece
                else
                    { piece
                        | position = calculateNewPoint position piece.mouseReference piece.position
                    }
            )


rotatePieces : Model -> KeyCode -> Logo
rotatePieces model keyCode =
    let
        list =
            model.elmLogo

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


movePiecesToCenter : Point -> Logo -> Logo
movePiecesToCenter distance elmLogo =
    let
        newPoint position =
            ( Tuple.first position + Tuple.first distance
            , Tuple.second position + Tuple.second distance
            )
    in
        elmLogo
            |> List.map
                (\piece ->
                    { piece
                        | position = newPoint piece.position
                        , mouseReference = Nothing
                    }
                )
