module Main exposing (main)

import Browser
import Browser.Events
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import SampleData exposing (miserablesGraph)
import Time
import TypedSvg exposing (circle, g, line, svg, title)
import TypedSvg.Attributes exposing (fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        graph =
            Graph.mapContexts initializeNode miserablesGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    ( Model Nothing graph (Force.simulation forces), Cmd.none )



-- UPDATE


type Msg
    = DragStart NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    ( { model
                        | graph = updateGraphWithList graph list
                        , simulation = newState
                      }
                    , Cmd.none
                    )

                Just { current, index } ->
                    ( { model
                        | graph =
                            Graph.update index
                                (Maybe.map (updateNode current))
                                (updateGraphWithList graph list)
                        , simulation = newState
                      }
                    , Cmd.none
                    )

        DragStart index xy ->
            ( { model | drag = Just (Drag xy xy index) }, Cmd.none )

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    ( { model
                        | drag = Just (Drag start xy index)
                        , graph = Graph.update index (Maybe.map (updateNode xy)) graph
                        , simulation = Force.reheat simulation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | drag = Nothing }, Cmd.none )

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    ( { model
                        | drag = Nothing
                        , graph = Graph.update index (Maybe.map (updateNode xy)) graph
                        , simulation = simulation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | drag = Nothing }, Cmd.none )



-- FORCE DIRECTED GRAPH


w : Float
w =
    990


h : Float
h =
    504


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none

            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Browser.Events.onAnimationFrame Tick
                ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Force Directed Graph"
    , body =
        [ siteHeader
        , node "main"
            []
            [ section
                []
                [ viewForce model
                ]
            ]
        , siteFooter
        ]
    }


siteHeader : Html Msg
siteHeader =
    Html.header [ class "site-header" ]
        [ h1 [] [ text "Force Directed Graph" ]
        ]


viewForce : Model -> Svg Msg
viewForce model =
    svg [ viewBox 0 0 w h ]
        [ Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ TypedSvg.Attributes.class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ TypedSvg.Attributes.class [ "nodes" ] ]
        ]


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement node =
    circle
        [ r 2.5
        , onMouseDown node.id
        , cx node.label.x
        , cy node.label.y
        ]
        [ title [] [ text node.label.value ] ]


siteFooter : Html Msg
siteFooter =
    footer [ class "site-footer" ]
        [ p [ class "copyright" ] [ text "© 2019 y047aka" ]
        ]
