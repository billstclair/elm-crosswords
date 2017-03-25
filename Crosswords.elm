module Crosswords exposing (..)

import Entities

import Html exposing ( Html, Attribute
                     , div, span, p, text, a, h2, h3
                     , table, tr, td
                     , input, button
                     )
import Html.Attributes exposing ( style, class, value, size
                                , type_, name, checked
                                )
import Html.Events exposing ( on, onClick, onInput )

import Svg exposing ( Svg, svg, line, rect, g )
import Svg.Attributes as SA
    exposing ( x, y, width, height
             , x1, y1, x2, y2
             , fill, stroke, fontSize, transform
             )
import List
import Array

log = Debug.log

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
        }

type alias Cell =
    { num : Int
    , opacity : Float
    , char : String
    }

type alias Board =
    List (List Cell)

makeBoard : (Int, Int) -> Board
makeBoard (width, height) =
    List.repeat height <| List.repeat width <| Cell 0 1.0 " "

type Direction
    = Across
    | Down

initialRows : List String
initialRows =
    [ "XOS BOW"
    , "    I I"
    , "    G N"
    ]

initialDims : Maybe (Int, Int)
initialDims =
    case List.head initialRows of
        Nothing ->
            Nothing
        Just row ->
            Just (String.length row, List.length initialRows)

initialBoard : Board
initialBoard =
    case initialDims of
        Nothing -> []
        Just dims ->
            List.map (\s ->
                          String.toList s
                          |> List.map String.fromChar
                          |> List.map (Cell 0 1.0)
                     )
                initialRows    

type alias Model =
    { dims : (Int, Int)
    , board : Board
    , cellSize : Int
    , newSize : String
    , word : String
    , start : (Int, Int)
    , direction : Direction
    }

defaultDims : (Int, Int)
defaultDims =
    (4, 4)

initialCellSize : Int
initialCellSize =
    100

initialModel : Model
initialModel =
    { dims = case initialDims of
                 Nothing -> defaultDims
                 Just dims -> dims
    , board = case initialDims of
                  Nothing -> makeBoard defaultDims
                  Just _ -> initialBoard
    , cellSize = initialCellSize
    , newSize = ""
    , word = "BOW"
    , start = (0, 0)
    , direction = Across
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )

type Msg
    = UpdateWord String
    | Fill
    | Erase
    | ChangeStart (Int, Int)
    | ChangeDirection Direction
    | BumpDims (Int, Int)
    | UpdateSize
    | UpdateNewSize String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
          UpdateWord word ->
              { model | word = word }
          Fill ->
              fill (String.toUpper model.word) model
          Erase ->
              erase model
          ChangeStart start ->
              { model | start = start }
          ChangeDirection direction ->
              { model | direction = direction }
          BumpDims delta ->
              bumpDims delta model
          UpdateSize ->
              updateSize model
          UpdateNewSize size ->
              { model | newSize = size }
    , Cmd.none
    )

bumpDims : (Int, Int) -> Model -> Model
bumpDims (dx, dy) model =
    let (x, y) = model.dims
        newx = min 10 <| max 2 <| x + dx
        newy = min 10 <| max 2 <| y + dy
        board = makeBoard (newx, newy)
        olda = Array.fromList <| List.map Array.fromList model.board
        newa = Array.fromList <| List.map Array.fromList board
        set = (\i j a ->
                   case Array.get j olda of
                       Nothing -> a
                       Just or ->
                           case Array.get i or of
                               Nothing -> a
                               Just x ->
                                   case Array.get j a of
                                       Nothing -> a
                                       Just r ->
                                           Array.set j (Array.set i x r) a
              )
        new2 = let loop = (\i j a ->
                               if i >= x then
                                   if j >= x then
                                       a
                                   else
                                       loop i (j+1) <| set i j a
                               else
                                   loop (i+1) j <| set i j a
                          )
               in
                   loop 0 0 newa
    in
        { model
            | dims = (newx, newy)
            , board = List.map Array.toList <| Array.toList new2
        }

updateSize : Model -> Model
updateSize model =
    case String.toInt model.newSize of
        Err _ ->
            model
        Ok size ->
            { model | cellSize = min 300 <| max 10 size }

fill : String -> Model -> Model
fill word model =
    let a = Array.fromList <| List.map Array.fromList model.board
        (x, y) = model.start
        (dx, dy) = case model.direction of
                       Across -> (1, 0)
                       Down -> (0, 1)
        loop = (\s (px, py) res ->
                    case s of
                        "" -> res
                        _ ->
                            case Array.get py res of
                                Nothing -> res
                                Just row ->
                                    loop (String.dropLeft 1 s)
                                        (px+dx, py+dy)
                                        <| Array.set py
                                            (Array.set
                                                 px
                                                 (Cell 0 1.0 <| String.left 1 s)
                                                 row)
                                            res
               )
        board = loop word model.start a
    in
        { model | board = List.map Array.toList <| Array.toList board }

erase : Model -> Model
erase model =
    fill (String.repeat (String.length model.word) " ") model

center : List (Attribute msg) -> List (Html msg) -> Html msg
center attributes body =
    Html.node "center" attributes body

br : Html msg
br =
    Html.br [] []

label : Attribute msg
label =
    style [ ("text-align", "right")
          , ("font-weight", "bold" )
          ]

radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    span [ onClick msg]
        [ input
            [ type_ "radio"
            , name "board-size"
            , checked isChecked
            ]
            []
        , text value
        ]

nbsp : Html msg
nbsp =
    text Entities.nbsp

view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "Crosswords" ]
        , div []
            [ table []
                  [ tr []
                        [ td [ label ] [ text "Rows:" ]
                        , td []
                            [ button [ onClick <| BumpDims (0, 1) ]
                                  [ text "+" ]
                            , nbsp
                            , text <| toString <| Tuple.second model.dims
                            , nbsp
                            , button [ onClick <| BumpDims (0, -1) ]
                                [ text "-" ]
                            ]
                        ]
                  , tr []
                      [ td [ label ] [ text "Cols:" ]
                      , td []
                          [ button [ onClick <| BumpDims (1, 0) ]
                                [ text "+" ]
                          , nbsp
                          , text <| toString <| Tuple.first model.dims
                          , nbsp
                          , button [ onClick <| BumpDims (-1, 0) ]
                              [ text "-" ]
                          ]
                      ]
                  , tr []
                      [ td [ label ] [ text "Cell Size:" ]
                      , td []
                          [ input [ value <| model.newSize
                                  , size 10
                                  , onInput UpdateNewSize
                                  ]
                                []
                          , nbsp
                          , button [ onClick UpdateSize ]
                              [ text "Change" ]
                          ]
                      ]
                  ]
            ]
        , br
        , div []
            [ drawBoard model ]
        , br
        , div []
            [ table []
                  [ tr []
                        [ td [ label ] [ text "Word:" ]
                        , td []
                            [ input [ value model.word
                                    , size 20
                                    , onInput UpdateWord
                                    ]
                                  []
                            , nbsp
                            , button [ onClick Fill ]
                                [ text "Fill" ]
                            , nbsp
                            , button [ onClick Erase ]
                                [ text "Erase" ]
                            ]
                        ]
                  , tr []
                      [ td [ label ] [ text "Direction:" ]
                      , td []
                          [  radio "Across" (model.direction == Across)
                              <| ChangeDirection Across
                          ,  nbsp, nbsp
                          , radio "Down" (model.direction == Down)
                                <| ChangeDirection Down
                          ]
                      ]
                  ]
            ]
        ]

svgClass : String -> Attribute msg
svgClass =
    SA.class

rectStyle : Attribute msg
rectStyle =
    style [ ( "fill", "white" )
          , ( "fill-opacity", "0" )
          , ( "stroke", "black" )
          ]

drawBoard : Model -> Html Msg
drawBoard model =
    let (dx, dy) = model.dims
        size = model.cellSize
        totalWidth = toString <| 3 + ((size+1) * dx)
        totalHeight = toString <| 3 + ((size+1) * dy)
    in
        svg [ width totalWidth, height totalHeight ]
            ( List.append
                  [ (rect [ rectStyle
                          , width totalWidth
                          , height totalHeight
                          ]
                         []
                    )
                  ]
                  (renderRows size model.board
                       <| filledCells
                           (String.length model.word)
                           model.start model.dims model.direction
                  )
            )

filledCells : Int -> (Int, Int) -> (Int, Int) -> Direction -> List (Int, Int)
filledCells count (sx, sy) (dx, dy) direction =
    if direction == Across then
        let mx = min dx (sx + count)
            loop = (\x cells ->
                        if x >= mx then
                            List.reverse cells
                        else
                            loop (x + 1) <| (x, sy) :: cells
                   )
        in
            loop sx []
    else
        let my = min dy (sy + count)
            loop = (\y cells ->
                        if y >= my then
                            List.reverse cells
                        else
                            loop (y + 1) <| (sx, y) :: cells
                   )
        in
            loop sy []

renderRows : Int -> Board -> List (Int, Int) -> List (Html Msg)
renderRows size board filled =
    let row : Int -> Int -> Board -> List (Html Msg) -> List (Html Msg)
        row = (\iy y rows res ->
                   case rows of
                       [] -> res
                       cells :: restRows ->
                           let cell : Int -> Int -> List Cell -> List (Html Msg) -> List (Html Msg)
                               cell = (\ix x cells res ->
                                           case cells of
                                               [] -> res
                                               c :: restCells ->
                                                   let opacity =
                                                           if List.member
                                                               (ix, iy) filled
                                                           then
                                                               0.05
                                                           else
                                                               0.0
                                                       stuff =
                                                           renderCell
                                                               c
                                                               x y
                                                               (toFloat size)
                                                               (ix, iy)
                                                               opacity
                                                   in
                                                       cell (ix + 1)
                                                           (x + size + 1)
                                                           restCells
                                                           <| List.append stuff res
                                      )
                               stuff = cell 0 2 cells []
                           in
                               row (iy + 1)
                                   (y + size + 1)
                                   restRows
                                   <| List.append stuff res
              )
    in
        row 0 2 board []

cellStyle : Float -> Attribute msg
cellStyle opacity =
    style [ ( "fill-opacity", toString opacity )
          , ( "fill", "black" )
          , ( "stroke", "black" )
          ]

textStyle : Attribute msg
textStyle =
    -- https://coderwall.com/p/a9nkrw/center-text-in-svg-rect-horz-and-vert
    style [ ( "text-anchor", "middle" )
          , ( "alignment-baseline", "central" )
          , ( "dominant-baseline", "central" )
          ]

renderCell : Cell -> Int -> Int -> Float -> (Int, Int) -> Float -> List (Svg Msg)
renderCell cell x y size pos opacity =
    [ Svg.text_ [ textStyle
                , SA.x (toString <| (toFloat x) + (size / 2.0))
                , SA.y (toString <| (toFloat y) + (size / 2.0))
                , fontSize <| toString <| size * 3.0 / 4.0
                ]
          [ Svg.text <| cell.char ]
    , rect [ cellStyle opacity
           , onClick <| ChangeStart pos
           , SA.x (toString x)
           , SA.y (toString y)
           , width (toString size)
           , height (toString size)
           ]
        []
    ]
