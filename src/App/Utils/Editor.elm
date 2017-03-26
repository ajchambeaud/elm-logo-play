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
    let
        newPosition =
            elmLogo
                |> .logo
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
    in
        { elmLogo | logo = newPosition }


findCenter : Logo -> Point
findCenter logo =
    logo.logo
        |> List.map .position
        |> List.foldl addPoint ( 0, 0 )
        |> scalePoint (1 / 7)


distanceToCenter : Logo -> Point
distanceToCenter =
    findCenter >> (diffPoint ( 400, 400 ))


selectPiece : Piece -> Logo -> Logo
selectPiece pieceSelected logo =
    let
        newLogo =
            logo.logo
                |> List.map
                    (\piece ->
                        if pieceSelected.figure == piece.figure then
                            { piece | selected = True }
                        else
                            { piece | selected = False }
                    )
    in
        { logo | logo = newLogo }


unselect : Logo -> Logo
unselect logo =
    let
        newLogo =
            logo.logo
                |> List.map
                    (\piece -> { piece | selected = False })
    in
        { logo | logo = newLogo }


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
    let
        elmLogo =
            model.elmLogo

        pieces =
            elmLogo.logo
                |> List.map
                    (\piece ->
                        if not piece.selected then
                            piece
                        else
                            { piece
                                | position = calculateNewPoint position piece.mouseReference piece.position
                            }
                    )
    in
        { elmLogo | logo = pieces }


rotatePieces : Model -> KeyCode -> Logo
rotatePieces model keyCode =
    let
        elmLogo =
            model.elmLogo

        list =
            elmLogo.logo

        rotate =
            case Key.fromCode keyCode of
                ArrowLeft ->
                    -(degrees 15)

                ArrowRight ->
                    degrees 15

                _ ->
                    degrees 0

        newPieces =
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
    in
        { elmLogo | logo = newPieces }


movePiecesToCenter : Point -> Logo -> Logo
movePiecesToCenter distance elmLogo =
    let
        newPoint position =
            ( Tuple.first position + Tuple.first distance
            , Tuple.second position + Tuple.second distance
            )

        pieces =
            elmLogo.logo
                |> List.map
                    (\piece ->
                        { piece
                            | position = newPoint piece.position
                            , mouseReference = Nothing
                        }
                    )
    in
        { elmLogo | logo = pieces }
