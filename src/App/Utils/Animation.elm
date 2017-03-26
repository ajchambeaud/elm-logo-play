module App.Utils.Animation exposing (..)

import App.Types exposing (..)
import Array exposing (..)
import Time exposing (Time)


moveV : Velocity
moveV =
    0.9


moveA : Aceleration
moveA =
    0.01


rotationV : Velocity
rotationV =
    0.4


rotationA : Aceleration
rotationA =
    0.01


originalLogo : Logo
originalLogo =
    { logo =
        [ Piece 0 (degrees 180) ( 500, 500 ) Nothing False
        , Piece 1 (degrees 270) ( 295, 495 ) Nothing False
        , Piece 2 (degrees 90) ( 505, 405 ) Nothing False
        , Piece 3 (degrees 0) ( 405, 345 ) Nothing False
        , Piece 4 (degrees 0) ( 355, 345 ) Nothing False
        , Piece 5 (degrees 0) ( 300, 290 ) Nothing False
        , Piece 6 (degrees 90) ( 505, 290 ) Nothing False
        ]
    , user = ""
    }


desarmedLogo : Logo
desarmedLogo =
    { logo =
        [ Piece 0 5.23 ( 260, 777 ) Nothing False
        , Piece 1 2.61 ( 150, 504 ) Nothing False
        , Piece 2 0.26 ( 703, 367 ) Nothing False
        , Piece 3 5.23 ( 617, 654 ) Nothing False
        , Piece 4 4.45 ( 328, 115 ) Nothing False
        , Piece 5 3.66 ( 121, 262 ) Nothing False
        , Piece 6 3.33 ( 616, 116 ) Nothing False
        ]
    , user = ""
    }


swanLogo : Logo
swanLogo =
    { logo =
        [ Piece 0 6.28 ( 218.42, 431.57 ) Nothing False
        , Piece 1 -0.78 ( 280.42, 577.57 ) Nothing False
        , Piece 2 7.06 ( 454.42, 252.57 ) Nothing False
        , Piece 3 0 ( 401.42, 359.57 ) Nothing False
        , Piece 4 1.57 ( 500.42, 416.57 ) Nothing False
        , Piece 5 1.57 ( 450.42, 253.57 ) Nothing False
        , Piece 6 8.63 ( 495.42, 508.57 ) Nothing False
        ]
    , user = ""
    }


nextCustomLogo : Logo -> List Logo -> Logo
nextCustomLogo previous listLogos =
    if previous.logo == originalLogo.logo then
        listLogos
            |> List.head
            |> Maybe.withDefault originalLogo
    else
        let
            isPrevious logo =
                logo.logo == previous.logo

            indexedPrev =
                listLogos
                    |> Array.fromList
                    |> Array.toIndexedList
                    |> List.filter (\item -> item |> Tuple.second |> isPrevious)
                    |> List.head
        in
            case indexedPrev of
                Nothing ->
                    originalLogo

                Just previousLogo ->
                    listLogos
                        |> Array.fromList
                        |> Array.get ((Tuple.first previousLogo) + 1)
                        |> Maybe.withDefault originalLogo


isAnimationFinish : Animation -> Bool
isAnimationFinish animation =
    animation.current.logo == animation.destiny.logo


nextAnimation : List Logo -> Animation -> Animation
nextAnimation customLogos animation =
    if animation.destiny.logo == desarmedLogo.logo then
        Animation desarmedLogo desarmedLogo (nextCustomLogo animation.origin customLogos)
    else
        Animation animation.destiny animation.destiny desarmedLogo


nextPosition : Time -> Velocity -> Aceleration -> NumAnimation -> Float
nextPosition dt v a { origin, current, destiny } =
    let
        dir =
            if origin < destiny then
                (+)
            else
                (-)

        newPoint =
            dir current (v * dt - 0.5 * a * (dt ^ 2))

        stop =
            (origin < destiny && newPoint > destiny) || (origin > destiny && newPoint < destiny) || (origin == destiny)
    in
        if stop then
            destiny
        else
            newPoint


rotatePiece : Time -> PieceAnimation -> PieceAnimation
rotatePiece dt ({ origin, current, destiny } as initial) =
    let
        position =
            NumAnimation origin.rotation current.rotation destiny.rotation
    in
        { initial
            | current =
                { current
                    | rotation = nextPosition dt rotationV rotationA position
                }
        }


movePiece : Time -> PieceAnimation -> PieceAnimation
movePiece dt ({ origin, current, destiny } as initial) =
    let
        positionX =
            NumAnimation (Tuple.first origin.position) (Tuple.first current.position) (Tuple.first destiny.position)

        positionY =
            NumAnimation (Tuple.second origin.position) (Tuple.second current.position) (Tuple.second destiny.position)
    in
        { initial
            | current =
                { current
                    | position = ( nextPosition dt moveV moveA positionX, nextPosition dt moveV moveA positionY )
                }
        }


moveAndRotate : Time -> Piece -> Piece -> Piece -> Piece
moveAndRotate dt origin current destiny =
    PieceAnimation origin current destiny
        |> movePiece dt
        |> rotatePiece dt
        |> .current


moveAnimationLogo : Animation -> Time -> Animation
moveAnimationLogo animation t =
    let
        newLogoPosition =
            List.map3
                (moveAndRotate t)
                animation.origin.logo
                animation.current.logo
                animation.destiny.logo
    in
        { animation
            | current = Logo newLogoPosition animation.destiny.user
        }


runAnimation : List Logo -> Animation -> Time -> Animation
runAnimation customLogos animation t =
    if isAnimationFinish animation then
        nextAnimation customLogos animation
    else
        moveAnimationLogo animation t
