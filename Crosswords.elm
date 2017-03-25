module Crosswords exposing (..)

import Entities

import Html exposing ( Html, Attribute
                     , div, span, p, text, a, h2, h3
                     , table, tr, td
                     , input, button
                     )
import Html.Attributes exposing ( style, class, value, size, href
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
import Array exposing ( Array )

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

type alias OneFill =
    { word : String
    , start : (Int, Int)
    , direction : Direction
    , cellNum : Int
    , cellOpacity : Float
    }

doOneFill : OneFill -> Model -> Model
doOneFill oneFill model =
    fill oneFill.word
        { model
            | word = oneFill.word
            , direction = oneFill.direction
            , start = oneFill.start
            , newNum = toString oneFill.cellNum
            , newOpacity = toString oneFill.cellOpacity
        }

initialFills : List OneFill
initialFills =
    [ OneFill "XOS" (0, 0) Down 1 0.2
    , OneFill "BIG" (2, 0) Down 2 0.2
    , OneFill "WIN" (4, 0) Down 3 0.2
    , OneFill "BOW" (2, 0) Across 2 1.0
    ]

type alias BoardArray =
    Array (Array Cell)

makeBoard : (Int, Int) -> Board
makeBoard (width, height) =
    List.repeat height <| List.repeat width <| Cell 0 1.0 " "

type Direction
    = Across
    | Down


type alias Model =
    { dims : (Int, Int)
    , board : Board
    , cellSize : Int
    , newSize : String
    , word : String
    , start : (Int, Int)
    , direction : Direction
    , cellNum : Int
    , newNum : String
    , cellOpacity : Float
    , newOpacity : String
    }

defaultDims : (Int, Int)
defaultDims =
    (5, 3)

initialCellSize : Int
initialCellSize =
    100

emptyModel : Model
emptyModel =
    { dims = defaultDims
    , board = makeBoard defaultDims
    , cellSize = initialCellSize
    , newSize = toString initialCellSize
    , word = "BOW"
    , start = (0, 0)
    , direction = Across
    , cellNum = 0
    , newNum = "0"
    , cellOpacity = 1.0
    , newOpacity = "1"
    }

initialModel : Model
initialModel =
    List.foldl doOneFill emptyModel initialFills

fromList : Board -> Array (Array Cell)
fromList board =
    Array.fromList <| List.map Array.fromList board

toList : Array (Array Cell) -> Board
toList array =
    List.map Array.toList <| Array.toList array

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
    | UpdateNum String
    | UpdateOpacity String

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
          UpdateNum string ->
              { model | newNum = string }
          UpdateOpacity string ->
              { model | newOpacity = string }
    , Cmd.none
    )

get : Int -> Int -> Array (Array e) -> Maybe e
get x y a =
    case Array.get y a of
        Nothing -> Nothing
        Just row ->
            Array.get x row

set : Int -> Int -> e -> Array (Array e) -> Array (Array e)
set x y elt a =
     case Array.get y a of
        Nothing -> a
        Just row ->
            Array.set y (Array.set x elt row) a
                                                    
xfer : Int -> Int -> Array (Array e) -> Array (Array e) -> Array (Array e)
xfer x y from to =
    case get x y from of
        Nothing -> to
        Just e ->
            set x y e to

bumpDims : (Int, Int) -> Model -> Model
bumpDims (dx, dy) model =
    let (x, y) = model.dims
        newx = min 10 <| max 2 <| x + dx
        newy = min 10 <| max 2 <| y + dy
        board = makeBoard (newx, newy)
        olda = fromList model.board
        newa = fromList board
        new2 = let loop : Int -> Int -> BoardArray -> BoardArray
                   loop = (\i j a ->
                               if j >= y then
                                   if i >= x then
                                       a
                                   else
                                       loop (i+1) 0 <| xfer i j olda a
                               else
                                   loop i (j+1) <| xfer i j olda a
                          )
               in
                   loop 0 0 newa
    in
        { model
            | dims = (newx, newy)
            , board = toList new2
        }

updateSize : Model -> Model
updateSize model =
    case String.toInt model.newSize of
        Err _ ->
            model
        Ok size ->
            let cellSize = min 300 <| max 10 size
            in
                { model
                    | cellSize = cellSize
                    , newSize = toString cellSize
                }

updateNum : Model -> Model
updateNum model =
    case String.toInt model.newNum of
        Err _ ->
            model
        Ok num ->
            { model | cellNum = min 9 <| max 0 num }

updateOpacity : Model -> Model
updateOpacity model =
    case String.toFloat model.newOpacity of
        Err _ ->
            model
        Ok opacity ->
            { model | cellOpacity = min 1.0 <| max 0.0 opacity }

fill : String -> Model -> Model
fill word mod =
    let model = updateNum <| updateOpacity mod
        a = fromList model.board
        (x, y) = model.start
        (dx, dy) = case model.direction of
                       Across -> (1, 0)
                       Down -> (0, 1)
        loop = (\s (px, py) res ->
                    case s of
                        "" -> res
                        _ ->
                            loop (String.dropLeft 1 s)
                                (px+dx, py+dy)
                                <| set px py
                                    (Cell (calculateCellNum px py model)
                                         model.cellOpacity
                                         <| String.left 1 s)
                                    res
               )
        board = loop word model.start a
    in
        { model
            | board = toList board
            , newNum = toString model.cellNum
            , newOpacity = toString model.cellOpacity
        }

calculateCellNum : Int -> Int -> Model -> Int
calculateCellNum x y model =
    if (x, y) == model.start then
        model.cellNum
    else
        case get x y <| fromList model.board of
            Nothing ->
                model.cellNum
            Just { num } ->
                num

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
                      [ td [ label ] [ text "Direction:" ]
                      , td []
                          [  radio "Across" (model.direction == Across)
                              <| ChangeDirection Across
                          ,  nbsp, nbsp
                          , radio "Down" (model.direction == Down)
                                <| ChangeDirection Down
                          ]
                      ]
                  , tr []
                      [ td [ label ] [ text "Clue Number:" ]
                      , td []
                          [ input [ value <| model.newNum
                                  , size 4
                                  , onInput UpdateNum
                                  ]
                                []
                          , text " (0 = none)"
                          ]
                      ]
                  , tr []
                      [ td [ label ] [ text "Opacity (0 to 1):" ]
                      , td []
                          [ input [ value <| model.newOpacity
                                  , size 4
                                  , onInput UpdateOpacity
                                  ]
                                []
                          , text " (0 = invisible, 1 = black)"
                          ]
                      ]
                  , tr []
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
                  ]
            ]
        , div []
            [ br
            , a [ href "https://github.com/billstclair/elm-crosswords" ]
                [ text "github.com/billstclair/elm-crosswords" ]
            , br, br
            , text <| "Copyright " ++ Entities.copyright ++ " 2017 Bill St. Clair"
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

textStyle : Float -> Attribute msg
textStyle opacity =
    -- https://coderwall.com/p/a9nkrw/center-text-in-svg-rect-horz-and-vert
    style [ ( "text-anchor", "middle" )
          , ( "alignment-baseline", "central" )
          , ( "dominant-baseline", "central" )
          , ( "opacity", toString opacity )
          ]

renderCell : Cell -> Int -> Int -> Float -> (Int, Int) -> Float -> List (Svg Msg)
renderCell cell x y size pos bgOpacity =
    let { char, num, opacity } = cell
        numText = if num == 0 then
                      []
                  else
                      [ Svg.text_ [ textStyle opacity
                                  , SA.x (toString <| (toFloat x) + (size / 8.0))
                                  , SA.y (toString <| (toFloat y) + (size / 8.0))
                                  , fontSize <| toString <| size * 1.0 / 5.0
                                  ]
                            [ Svg.text <| toString num ]
                      ]
    in
        List.append
            numText
            [ Svg.text_ [ textStyle opacity
                        , SA.x (toString <| (toFloat x) + (size / 2.0))
                        , SA.y (toString <| (toFloat y) + (size / 2.0))
                        , fontSize <| toString <| size * 3.0 / 4.0
                        ]
                  [ Svg.text <| cell.char ]
            , rect [ cellStyle bgOpacity
                   , onClick <| ChangeStart pos
                   , SA.x (toString x)
                   , SA.y (toString y)
                   , width (toString size)
                   , height (toString size)
                   ]
                  []
            ]
