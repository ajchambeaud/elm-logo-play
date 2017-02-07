module App.State exposing (..)

import App.Utils.Animation exposing (isAnimationFinish, nextAnimation, runAnimation, originalLogo, desarmedLogo)
import App.Utils.Editor exposing (setPositionFix, selectPiece, unselect, updatePieces, rotatePieces, distanceToCenter, movePiecesToCenter)
import App.Types exposing (..)
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import AnimationFrame
import Time exposing (Time)


initCmd : Cmd Msg
initCmd =
    Cmd.none


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


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPosition position ->
            ( setPosition model position
            , Cmd.none
            )

        SetAction action ->
            ( setAction model action
            , Cmd.none
            )

        SelectForm piece ->
            ( selectForm model piece
            , Cmd.none
            )

        LeaveForm position ->
            ( leaveForm model position
            , Cmd.none
            )

        Move position ->
            ( move model position
            , Cmd.none
            )

        KeyPress keyCode ->
            ( rotate model keyCode
            , Cmd.none
            )

        Reset ->
            ( reset model
            , Cmd.none
            )

        Edit ->
            ( setEditMode model
            , Cmd.none
            )

        Cancel ->
            ( cancel model
            , Cmd.none
            )

        Save ->
            ( save model
            , Cmd.none
            )

        Tick time ->
            ( counter model
            , Cmd.none
            )

        TimeUpdate time ->
            ( animateModel time model
            , Cmd.none
            )


setPosition : Model -> Position -> Model
setPosition model position =
    { model
        | elmLogo = setPositionFix position model.elmLogo
    }


setAction : Model -> Action -> Model
setAction model action =
    { model
        | action = action
        , elmLogo = unselect model.elmLogo
        , animate = False
    }


selectForm : Model -> Piece -> Model
selectForm model piece =
    case model.action of
        NoAction ->
            model

        _ ->
            { model
                | elmLogo = selectPiece piece model.elmLogo
                , active = True
            }


leaveForm : Model -> Position -> Model
leaveForm model position =
    case model.action of
        Drag ->
            { model
                | elmLogo = unselect model.elmLogo
                , active = False
            }

        _ ->
            model


move : Model -> Position -> Model
move model position =
    case model.action of
        Drag ->
            { model
                | elmLogo = updatePieces model position
            }

        _ ->
            model


rotate : Model -> KeyCode -> Model
rotate model keyCode =
    case model.action of
        Rotate ->
            { model | elmLogo = rotatePieces model keyCode }

        _ ->
            model


reset : Model -> Model
reset model =
    { model | elmLogo = initModel.elmLogo }


setEditMode : Model -> Model
setEditMode model =
    { model
        | action = Drag
        , elmLogo = originalLogo
        , animation = initModel.animation
        , animate = False
    }


cancel : Model -> Model
cancel model =
    { model
        | elmLogo = initModel.elmLogo
        , action = NoAction
    }


save : Model -> Model
save model =
    { model
        | action = NoAction
        , elmLogo = initModel.elmLogo
        , customLogos = (movePiecesToCenter (distanceToCenter model.elmLogo) model.elmLogo) :: model.customLogos
    }


counter : Model -> Model
counter model =
    case model.action of
        NoAction ->
            { model
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

        _ ->
            model


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
