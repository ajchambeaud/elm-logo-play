module App.Utils.LogoForms exposing (logoForms, emptyPoint)

import Color exposing (..)
import Graphics.Render exposing (..)
import Array exposing (..)


elmBlue : Color
elmBlue =
    rgb 90 99 120


elmLightblue : Color
elmLightblue =
    rgb 96 181 204


elmOrange : Color
elmOrange =
    rgb 240 173 0


elmGreen : Color
elmGreen =
    rgb 127 209 59


emptyPoint : Form msg
emptyPoint =
    circle 0 |> filled (solid (rgb 0 0 0))


isosceles : Float -> Float -> Shape
isosceles base height =
    polygon
        [ ( 0, 0 )
        , ( base, 0 )
        , ( base / 2, height )
        ]


ractangle : Float -> Float -> Shape
ractangle base height =
    polygon
        [ ( 0, 0 )
        , ( base, 0 )
        , ( 0, height )
        ]


elmPolygon : Float -> Float -> Shape
elmPolygon base height =
    polygon
        [ ( 0, 0 )
        , ( base, 0 )
        , ( base / 2 + base, height )
        , ( base / 2, height )
        ]


logoForm1 : Form msg
logoForm1 =
    isosceles 200 100
        |> filled (solid elmLightblue)


logoForm2 : Form msg
logoForm2 =
    isosceles 200 100
        |> filled (solid elmBlue)


logoForm3 : Form msg
logoForm3 =
    isosceles 90 45
        |> filled (solid elmOrange)


logoForm4 : Form msg
logoForm4 =
    polygon
        [ ( 50, 0 )
        , ( 100, 50 )
        , ( 50, 100 )
        , ( 0, 50 )
        ]
        |> filled (solid elmGreen)


logoForm5 : Form msg
logoForm5 =
    isosceles 90 45
        |> filled (solid elmOrange)


logoForm6 : Form msg
logoForm6 =
    elmPolygon 100 50
        |> filled (solid elmGreen)


logoForm7 : Form msg
logoForm7 =
    ractangle 95 95
        |> filled (solid elmLightblue)


logoForms : Array (Form msg)
logoForms =
    Array.fromList
        [ logoForm1
        , logoForm2
        , logoForm3
        , logoForm4
        , logoForm5
        , logoForm6
        , logoForm7
        ]
