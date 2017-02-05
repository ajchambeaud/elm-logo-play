module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, attribute, src)
import Html.Events exposing (..)
import Graphics.Render exposing (Point, Form, angle, group, svg, position, opacity)
import LogoForms exposing (logoForms, emptyPoint)
import MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)
import Keyboard exposing (KeyCode)
import Key exposing (..)
import Mouse exposing (Position)
import AnimationFrame
import Time exposing (Time)
import Array exposing (..)


type Action
    = Drag
    | Rotate
    | NoAction


type Msg
    = TimeUpdate Time
    | Tick Time
    | SelectForm Piece
    | LeaveForm Position
    | Move Position
    | SetPosition Position
    | SetAction Action
    | KeyPress KeyCode
    | Reset
    | Edit
    | Cancel
    | Save


type alias Piece =
    { figure : Int
    , rotation : Float
    , position : Point
    , mouseReference : Maybe Position
    , selected : Bool
    }


type alias Logo =
    List Piece


type alias Animation =
    { origin : Logo
    , current : Logo
    , destiny : Logo
    }


type alias PieceAnimation =
    { origin : Piece
    , current : Piece
    , destiny : Piece
    }


type alias Model =
    { elmLogo : Logo
    , customLogos : List Logo
    , action : Action
    , active : Bool
    , animation : Animation
    , counter : Int
    , animate : Bool
    }


nextCustomLogo : Logo -> List Logo -> Logo
nextCustomLogo previous listLogos =
    if previous == originalLogo then
        listLogos
            |> List.head
            |> Maybe.withDefault originalLogo
    else
        let
            isPrevious logo =
                logo == previous

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
    animation.current == animation.destiny


nextAnimation : List Logo -> Animation -> Animation
nextAnimation customLogos animation =
    if animation.destiny == desarmedLogo then
        Animation desarmedLogo desarmedLogo (nextCustomLogo animation.origin customLogos)
    else
        Animation animation.destiny animation.destiny desarmedLogo


nextPosition : Float -> Float -> Float -> Float
nextPosition origin destiny current =
    let
        stop =
            (origin < destiny && current > destiny) || (origin > destiny && current < destiny) || (origin == destiny)
    in
        if stop then
            destiny
        else
            current


rotatePiece : Time -> PieceAnimation -> PieceAnimation
rotatePiece dt ({ origin, current, destiny } as initial) =
    let
        a =
            0.01

        v =
            0.4

        dir =
            if origin.rotation < destiny.rotation then
                (+)
            else
                (-)

        x =
            dir current.rotation (v * dt - 0.5 * a * (dt ^ 2))
    in
        { initial
            | current =
                { current
                    | rotation = nextPosition origin.rotation destiny.rotation x
                }
        }


movePiece : Time -> PieceAnimation -> PieceAnimation
movePiece dt ({ origin, current, destiny } as initial) =
    let
        a =
            0.01

        v =
            0.9

        x0 =
            Tuple.first current.position

        x1 =
            Tuple.first destiny.position

        y0 =
            Tuple.second current.position

        y1 =
            Tuple.second destiny.position

        xOrigin =
            Tuple.first origin.position

        yOrigin =
            Tuple.second origin.position

        xDir =
            if xOrigin < x1 then
                (+)
            else
                (-)

        yDir =
            if yOrigin < y1 then
                (+)
            else
                (-)

        x =
            xDir x0 (v * dt - 0.5 * a * (dt ^ 2))

        y =
            yDir y0 (v * dt - 0.5 * a * (dt ^ 2))
    in
        { initial
            | current =
                { current
                    | position = ( nextPosition xOrigin x1 x, nextPosition yOrigin y1 y )
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
                animation.origin
                animation.current
                animation.destiny
    in
        { animation
            | current = newLogoPosition
        }


runAnimation : List Logo -> Animation -> Time -> Animation
runAnimation customLogos animation t =
    if isAnimationFinish animation then
        nextAnimation customLogos animation
    else
        moveAnimationLogo animation t


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


originalLogo : Logo
originalLogo =
    [ Piece 0 (degrees 180) ( 500, 500 ) Nothing False
    , Piece 1 (degrees 270) ( 295, 495 ) Nothing False
    , Piece 2 (degrees 90) ( 505, 405 ) Nothing False
    , Piece 3 (degrees 0) ( 405, 345 ) Nothing False
    , Piece 4 (degrees 0) ( 355, 345 ) Nothing False
    , Piece 5 (degrees 0) ( 300, 290 ) Nothing False
    , Piece 6 (degrees 90) ( 505, 290 ) Nothing False
    ]


desarmedLogo : Logo
desarmedLogo =
    [ Piece 0 5.23 ( 260, 777 ) Nothing False
    , Piece 1 2.61 ( 150, 504 ) Nothing False
    , Piece 2 0.26 ( 703, 367 ) Nothing False
    , Piece 3 5.23 ( 617, 654 ) Nothing False
    , Piece 4 4.45 ( 328, 115 ) Nothing False
    , Piece 5 3.66 ( 121, 262 ) Nothing False
    , Piece 6 3.33 ( 616, 116 ) Nothing False
    ]


initModel : Model
initModel =
    { elmLogo = originalLogo
    , customLogos = []
    , action = NoAction
    , active = False
    , animation = Animation originalLogo originalLogo desarmedLogo
    , counter = 0
    , animate = False
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( initModel, initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.action, msg ) of
        ( _, SetPosition position ) ->
            ( { model | elmLogo = setPositionFix position model.elmLogo }
            , Cmd.none
            )

        ( _, SetAction action ) ->
            ( { model
                | action = action
                , elmLogo = unselect model.elmLogo
                , animate = False
              }
            , Cmd.none
            )

        ( Drag, SelectForm piece ) ->
            ( { model
                | elmLogo = selectPiece piece model.elmLogo
                , active = True
              }
            , Cmd.none
            )

        ( Rotate, SelectForm piece ) ->
            ( { model
                | elmLogo = selectPiece piece model.elmLogo
                , active = True
              }
            , Cmd.none
            )

        ( Drag, LeaveForm position ) ->
            ( { model
                | elmLogo = unselect model.elmLogo
                , active = False
              }
            , Cmd.none
            )

        ( Drag, Move position ) ->
            ( { model | elmLogo = updatePieces model position }
            , Cmd.none
            )

        ( Rotate, KeyPress keyCode ) ->
            ( { model | elmLogo = rotatePieces model keyCode }
            , Cmd.none
            )

        ( _, Reset ) ->
            ( { model | elmLogo = initModel.elmLogo }
            , Cmd.none
            )

        ( _, Edit ) ->
            ( { model
                | action = Drag
                , elmLogo = originalLogo
                , animation = initModel.animation
                , animate = False
              }
            , Cmd.none
            )

        ( _, Cancel ) ->
            ( { model
                | elmLogo = initModel.elmLogo
                , action = NoAction
              }
            , Cmd.none
            )

        ( _, Save ) ->
            ( { model
                | action = NoAction
                , elmLogo = initModel.elmLogo
                , customLogos = (movePiecesToCenter (distanceToCenter model.elmLogo) model.elmLogo) :: model.customLogos
              }
            , Cmd.none
            )

        ( NoAction, Tick time ) ->
            ( { model
                | counter =
                    if model.counter == 2 then
                        0
                    else
                        (model.counter + 1)
                , animate =
                    if model.counter == 2 || model.animation.current == desarmedLogo then
                        True
                    else
                        False
                , animation =
                    if isAnimationFinish model.animation then
                        nextAnimation model.customLogos model.animation
                    else
                        model.animation
              }
            , Cmd.none
            )

        ( NoAction, TimeUpdate time ) ->
            ( animateModel time model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


animateModel : Time -> Model -> Model
animateModel t model =
    let
        animation =
            runAnimation model.customLogos model.animation t
    in
        { model
            | counter = 0
            , elmLogo = animation.current
            , animation = animation
            , animate =
                if isAnimationFinish animation && animation.current /= desarmedLogo then
                    False
                else
                    True
        }


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
            [ Html.text "Move piece" ]
        , button
            [ classList
                [ ( "btn", True )
                , ( "btn-rotate", True )
                , ( "active", model.action == Rotate )
                ]
            , onClick (SetAction Rotate)
            ]
            [ Html.text "Rotate piece" ]
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
        , model.elmLogo
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
                , onClick Edit
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


forkMeRibbon : Html Msg
forkMeRibbon =
    a [ href "https://github.com/ajchambeaud/elm-logo-play" ]
        [ img [ attribute "data-canonical-src" "https://s3.amazonaws.com/github/ribbons/forkme_left_orange_ff7600.png", src "https://camo.githubusercontent.com/8b6b8ccc6da3aa5722903da7b58eb5ab1081adee/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f6c6566745f6f72616e67655f6666373630302e706e67", attribute "style" "position: absolute; top: 0; left: 0; border: 0;" ]
            []
        ]


credits : Html Msg
credits =
    div [ class "credits" ]
        [ a [ class "twitter", href "https://twitter.com/ajchambeaud" ] [ Html.text "Made with 🌳 by @ajchambeaud" ]
        ]


sidebar : Model -> Html Msg
sidebar model =
    div [ class "sidebar" ]
        [ h1 []
            [ Html.text "elm-logo.play" ]
        , controlls model
        , credits
        , forkMeRibbon
        ]


view : Model -> Html Msg
view model =
    div [ class "grid" ]
        [ sidebar model
        , collage model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.active, model.action, model.animate ) of
        ( False, Drag, False ) ->
            Sub.batch [ Mouse.downs SetPosition ]

        ( True, Drag, False ) ->
            Sub.batch
                [ Mouse.moves Move
                , Mouse.ups LeaveForm
                , Mouse.downs SetPosition
                ]

        ( False, Rotate, False ) ->
            Sub.none

        ( True, Rotate, False ) ->
            Sub.batch
                [ Keyboard.downs KeyPress
                ]

        ( _, NoAction, True ) ->
            Sub.batch
                [ AnimationFrame.diffs TimeUpdate
                ]

        ( _, NoAction, False ) ->
            Sub.batch
                [ Time.every Time.second Tick
                ]

        _ ->
            Sub.none
