module MouseEvents exposing (onMouseMove, onDragEnd, onDragStart)

import Html.Events exposing (..)
import Json.Decode as Json exposing (..)
import Graphics.Render exposing (..)


on : String -> Json.Decoder msg -> Form msg -> Form msg
on event decoder f =
    { f | handlers = ( event, decoder ) :: f.handlers }


simpleOn : String -> msg -> Form msg -> Form msg
simpleOn event =
    on event << Json.succeed


mouseOn : String -> (Point -> msg) -> Form msg -> Form msg
mouseOn event msg =
    on event <|
        Json.map msg <|
            Json.map2
                (\x y -> ( x, y ))
                (field "clientX" Json.float)
                (field "clientY" Json.float)


onMouseMove : (Point -> msg) -> Form msg -> Form msg
onMouseMove =
    mouseOn "mousemove"


onDragEnd : msg -> Form msg -> Form msg
onDragEnd =
    simpleOn "mouseup"


onDragStart : msg -> Form msg -> Form msg
onDragStart =
    simpleOn "mousedown"
