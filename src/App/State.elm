port module App.State exposing (..)

import App.Utils.Animation exposing (isAnimationFinish, nextAnimation, runAnimation, originalLogo, desarmedLogo, swanLogo)
import App.Utils.Editor exposing (setPositionFix, selectPiece, unselect, updatePieces, rotatePieces, distanceToCenter, movePiecesToCenter)
import App.Types exposing (..)
import Keyboard exposing (KeyCode)
import Mouse exposing (Position)
import AnimationFrame
import Time exposing (Time)
import Json.Decode exposing (int, float, list, maybe, bool, string, nullable, Decoder, Value, oneOf, null, index, andThen, succeed)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


initialCmd : Cmd Msg
initialCmd =
    Cmd.none


initialModel : Model
initialModel =
    { elmLogo = originalLogo
    , customLogos = []
    , action = NoAction
    , active = False
    , animation = Animation originalLogo originalLogo desarmedLogo
    , counter = 0
    , animate = False
    , user = Nothing
    , loginError = Nothing
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init user =
    case Json.Decode.decodeValue (nullOr userDecoder) user of
        Err err ->
            ( { initialModel | loginError = Just err }, initialCmd )

        Ok user ->
            ( { initialModel | user = user }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserError err ->
            ( { model | loginError = Just err }, Cmd.none )

        UserUpdate user ->
            ( { model | user = Just user }, Cmd.none )

        Login ->
            ( model, login "" )

        Logout ->
            ( { model | user = Nothing }, logout "" )

        CloseAlert ->
            ( { model | loginError = Nothing }, logout "" )

        UpdateLogoListError err ->
            ( { model | loginError = Just err }, Cmd.none )

        UpdateLogoList list ->
            ( { model | customLogos = list }, Cmd.none )

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
            , saveCustomLogo model
            )

        Tick time ->
            ( counter model
            , Cmd.none
            )

        TimeUpdate time ->
            ( animateModel time model
            , Cmd.none
            )


saveCustomLogo : Model -> Cmd Msg
saveCustomLogo model =
    case model.user of
        Just user ->
            savePort <| movePiecesToCenter (distanceToCenter model.elmLogo) model.elmLogo

        Nothing ->
            Cmd.none


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
    { model | elmLogo = initialModel.elmLogo }


setEditMode : Model -> Model
setEditMode model =
    case model.user of
        Just user ->
            { model
                | action = Drag
                , elmLogo = Maybe.withDefault { originalLogo | user = user.name } user.customLogo
                , animation = initialModel.animation
                , animate = False
            }

        Nothing ->
            model


cancel : Model -> Model
cancel model =
    { model
        | elmLogo = initialModel.elmLogo
        , action = NoAction
    }


save : Model -> Model
save model =
    { model
        | action = NoAction
        , elmLogo = initialModel.elmLogo
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


port login : String -> Cmd msg


port logout : String -> Cmd msg


port savePort : Logo -> Cmd msg


port loginSuccess : (Value -> msg) -> Sub msg


port customLogosUpdated : (Value -> msg) -> Sub msg


port loginError : (String -> msg) -> Sub msg


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , Json.Decode.map Just decoder
        ]


decodePosition : Decoder Position
decodePosition =
    decode Position
        |> required "x" int
        |> required "y" int


arrayAsTuple2 : Decoder a -> Decoder a -> Decoder ( a, a )
arrayAsTuple2 a b =
    index 0 a
        |> andThen
            (\aVal ->
                index 1 b
                    |> andThen (\bVal -> succeed ( aVal, bVal ))
            )


decodePiece : Decoder Piece
decodePiece =
    decode Piece
        |> required "figure" int
        |> required "rotation" float
        |> required "position" (arrayAsTuple2 float float)
        |> optional "mouseReference" (maybe decodePosition) Nothing
        |> required "selected" bool


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "uid" string
        |> required "displayName" string
        |> required "customLogo" (maybe customLogosDecoder)


customLogosDecoder : Decoder Logo
customLogosDecoder =
    decode Logo
        |> required "logo" (list decodePiece)
        |> required "user" string


decodeListResponse : Json.Decode.Value -> Msg
decodeListResponse json =
    case Json.Decode.decodeValue (list customLogosDecoder) json of
        Err err ->
            UpdateLogoListError err

        Ok logos ->
            UpdateLogoList logos


decodeUserResponse : Json.Decode.Value -> Msg
decodeUserResponse json =
    case Json.Decode.decodeValue userDecoder json of
        Err err ->
            UserError err

        Ok user ->
            UserUpdate user


loginSubscriptions : List (Sub Msg)
loginSubscriptions =
    [ loginSuccess decodeUserResponse
    , loginError UserError
    , customLogosUpdated decodeListResponse
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.active, model.action, model.animate ) of
        ( False, Drag, False ) ->
            Mouse.downs SetPosition
                :: loginSubscriptions
                |> Sub.batch

        ( True, Drag, False ) ->
            [ Mouse.moves Move
            , Mouse.ups LeaveForm
            , Mouse.downs SetPosition
            ]
                |> List.append loginSubscriptions
                |> Sub.batch

        ( False, Rotate, False ) ->
            Sub.batch loginSubscriptions

        ( True, Rotate, False ) ->
            (Keyboard.downs KeyPress)
                :: loginSubscriptions
                |> Sub.batch

        ( _, NoAction, True ) ->
            (AnimationFrame.diffs TimeUpdate)
                :: loginSubscriptions
                |> Sub.batch

        ( _, NoAction, False ) ->
            (Time.every Time.second Tick)
                :: loginSubscriptions
                |> Sub.batch

        _ ->
            Sub.batch loginSubscriptions
