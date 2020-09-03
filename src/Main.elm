module Main exposing (main)

import Array exposing (Array)
import Array.Extra as Array
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


type alias Model =
    { grid : Grid Cell }


init : Model
init =
    { grid = initGrid 100 Dead }



--initGrid creates a n by n grid with each cell initilized with the value v


initGrid : Int -> a -> Grid a
initGrid n v =
    let
        rows =
            Array.initialize n (always v)
    in
    Array.initialize n (always rows)


type Msg
    = Toggle Int Int
    | NextGen


type Cell
    = Alive
    | Dead


type alias Grid a =
    Array (Array a)


test : Int -> Int
test x =
    x * x



-- renderGrid takes a grid and gives a svg message
-- with w times h many cells, the hwole thing having a size of gs (grid size)
-- coloring them according to their content
-- the point of w and h is that we might want to not render the whole grid


gridSize : Int -> Int -> Int -> List (Svg.Attribute msg)
gridSize w h s =
    [ width (String.fromInt (w * s)), height (String.fromInt (h * s)) ]


renderGrid : Grid Cell -> Int -> Int -> Int -> List (Svg Msg)
renderGrid grid wid hei gs =
    let
        --making sure we don't try tro render more cells that there are
        w =
            -- number of cells
            Basics.min wid (Array.length grid)

        h =
            -- it should always be h=w but I'll leave this in for now
            Basics.min hei (Array.length grid)
    in
    --svg [ width (String.fromInt gs), height (String.fromInt gs) ]
    --(gridSize w h s)
    --width (String.fromInt (w * s)), height (String.fromInt (h * s)) ]
    grid
        |> Array.indexedMap (\x -> Array.indexedMap (\y -> createRect x y (gs // w)))
        |> Array.toList
        |> List.map Array.toList
        |> List.concat



{--(Array.indexedMap
            (\x -> Array.indexedMap (createRect x s))
            grid
        |> Array.toList
        |> List.map Array.toList --}
-- Here goes the createRect part
--createRect creates a rectangle of size s at coordinates xC yC with content c


createRect : Int -> Int -> Int -> Cell -> Svg Msg
createRect xC yC s c =
    rect
        [ x (String.fromInt (s * xC))
        , y (String.fromInt (s * yC))
        , width (String.fromInt s)
        , height (String.fromInt s)
        , fill (colour c)
        , stroke "black"
        , Svg.Events.onClick (Toggle yC xC)
        ]
        []



{--
    rect
        [ x (String.fromInt c.xC)
        , y (String.fromInt c.xY)
        , width (String.fromInt s)
        , height (String.fromInt s)
        , fill (colour c.content)
        , stroke "black"
        , Svg.Events.onClick Toggle
        ]
        [] --}


colour : Cell -> String
colour m =
    if m == Alive then
        "black"

    else
        "white"


view : Model -> Html Msg
view model =
    div []
        [ svg [ width (String.fromInt 500), height (String.fromInt 500), viewBox "0 0 200 200" ]
            (renderGrid model.grid 100 100 500)
        , button [ Html.Events.onClick NextGen ] [ Html.text "Next Generation" ]
        ]



{--svg
            [ width "300"
            , height "150"
            , viewBox "5 0 300 100"
            ]
            [ rect
                [ x "9"
                , width "100"
                , height "100"
                , fill (colour model)
                , stroke "black"
                , Svg.Events.onClick Toggle
                ]
                []
            , rect
                [ x "110"
                , width "100"
                , height "100"
                , fill "red"
                , stroke "black"
                , Svg.Events.onClick Toggle
                ]
                []
            ]--}


mapAt : Int -> Int -> (a -> a) -> Grid a -> Grid a
mapAt x y f grid =
    Array.update y (Array.update x f) grid


setAt : Int -> Int -> a -> Grid a -> Grid a
setAt x y value grid =
    mapAt x y (always value) grid


getAt : Int -> Int -> Grid a -> Maybe a
getAt x y grid =
    grid
        |> Array.get y
        |> Maybe.andThen (Array.get x)


evalCellinGrid : Int -> Int -> Grid Cell -> Int
evalCellinGrid x y grid =
    let
        value =
            grid
                |> getAt x y
                |> Maybe.withDefault Dead
    in
    case value of
        Alive ->
            1

        Dead ->
            0


countNeighbors : Int -> Int -> Grid Cell -> Int
countNeighbors x y grid =
    let
        neighbors =
            [ evalCellinGrid (x + 1) (y + 1) grid
            , evalCellinGrid x (y + 1) grid
            , evalCellinGrid (x - 1) (y + 1) grid
            , evalCellinGrid (x + 1) y grid
            , evalCellinGrid (x - 1) y grid
            , evalCellinGrid (x + 1) (y - 1) grid
            , evalCellinGrid x (y - 1) grid
            , evalCellinGrid (x - 1) (y - 1) grid
            ]
    in
    List.sum neighbors


toggle : Cell -> Cell
toggle c =
    case c of
        Alive ->
            Dead

        Dead ->
            Alive


rules : Int -> Cell -> Cell
rules numOfNeighbors cell =
    case cell of
        Alive ->
            if numOfNeighbors < 2 then
                Dead

            else if numOfNeighbors > 3 then
                Dead

            else
                Alive

        Dead ->
            if numOfNeighbors == 3 then
                Alive

            else
                Dead



{--
updateCell : ( Int, Int ) -> Grid Cell -> Grid Cell
updateCell point grid =
    mapAt (Tuple.first point) (Tuple.second point) (rules (countNeighbors (Tuple.first point) (Tuple.second point) grid))
--}


nextGenOfCell : Int -> Int -> Grid Cell -> Maybe Cell
nextGenOfCell x y grid =
    let
        cell =
            getAt x y grid
    in
    case cell of
        Nothing ->
            Nothing

        Just c ->
            Just (rules (countNeighbors x y grid) c)


nextGenOfCellWithDefault : ( Int, Int ) -> Grid Cell -> Cell
nextGenOfCellWithDefault point grid =
    Maybe.withDefault Dead (nextGenOfCell (Tuple.first point) (Tuple.second point) grid)


writeGenIntoGrid : Grid Cell -> ( Int, Int ) -> Grid Cell -> Grid Cell
writeGenIntoGrid grid point target =
    setAt (Tuple.first point) (Tuple.second point) (nextGenOfCellWithDefault point grid) target


nextGeneration : Grid Cell -> Grid Cell
nextGeneration grid =
    let
        l =
            Array.length grid

        emptyGrid =
            initGrid l Dead

        indizes =
            List.concat (List.map (\x -> List.map (\y -> ( x, y )) (List.range 0 (l - 1))) (List.range 0 (l - 1)))
    in
    List.foldr (writeGenIntoGrid grid) emptyGrid indizes


update : Msg -> Model -> Model
update msg model =
    case msg of
        Toggle x y ->
            { model | grid = mapAt x y toggle model.grid }

        NextGen ->
            { model | grid = nextGeneration model.grid }


main =
    Browser.sandbox { init = init, update = update, view = view }
