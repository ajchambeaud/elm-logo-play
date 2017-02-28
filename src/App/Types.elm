module App.Types exposing (..)

import Mouse exposing (Position)
import Graphics.Render exposing (Point)
import Time exposing (Time)
import Keyboard exposing (KeyCode)


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
    | UserError String
    | UserUpdate User
    | Login
    | Logout


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


type alias NumAnimation =
    { origin : Float
    , current : Float
    , destiny : Float
    }


type alias User =
    { id : String
    , name : String
    }


type alias Model =
    { elmLogo : Logo
    , customLogos : List Logo
    , action : Action
    , active : Bool
    , animation : Animation
    , counter : Int
    , animate : Bool
    , user : Maybe User
    , loginError : Maybe String
    }


type alias Velocity =
    Float


type alias Aceleration =
    Float
