module Main exposing (Event, Memory, PageView, main)

import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Process
import Task
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Procedure as Procedure exposing (Block, Procedure)
import Time exposing (Posix)


main : Program () Memory Event
main =
    Browser.document
        { init = init
        , procedures = procedures
        , view = Browser.globalDocument view
        , subscriptions = Browser.globalSubscriptions subscriptions
        }


{-| The memory state shared by all threads.
-}
type alias Memory =
    { log : String
    , page : PageView
    }


init : Memory
init =
    { log = ""
    , page = PageLoading
    }


{-| Events that only affect a specific thread.
-}
type Event
    = ReceiveTick Posix
    | ClickActionButton
    | ReceiveInitialTime ( Time.Zone, Posix )
    | WakeUp



-- View


type PageView
    = PageLoading
    | PageHome PageHome_


view : Memory -> Document Event
view memory =
    case memory.page of
        PageLoading ->
            pageLoading

        PageHome home ->
            pageHome memory.log home


pageLoading : Document msg
pageLoading =
    { title = "Sample application -- Loading"
    , body =
        [ Html.div
            [ style "padding" "0.6em"
            , style "margin" "0"
            ]
            [ Html.text "Loading..."
            ]
        ]
    }


type alias PageHome_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


pageHome : String -> PageHome_ -> Document Event
pageHome log home =
    { title = "Sample application -- Home"
    , body =
        [ Html.div
            [ style "padding" "0.3em"
            , style "margin" "0"
            ]
            [ Html.div
                [ style "padding" "0.3em"
                , style "margin" "0"
                ]
                [ Html.span []
                    [ Html.text <| "Current time: " ++ formatTime home.zone home.time
                    ]
                ]
            , Html.div
                [ style "padding" "0.3em"
                , style "margin" "0"
                ]
                [ Html.pre
                    [ style "padding" "0.6em"
                    , style "margin" "0"
                    , style "max-width" "18em"
                    , style "height" "16em"
                    , style "overflow-y" "auto"
                    , style "border" "solid black 1px"
                    ]
                    [ Html.text log
                    ]
                ]
            , if home.showActionButton then
                Html.div
                    [ style "padding" "0.3em"
                    , style "margin" "0"
                    ]
                    [ Html.button
                        [ Attributes.type_ "button"
                        , Events.onClick ClickActionButton
                        ]
                        [ Html.text "Action"
                        ]
                    ]

              else
                Html.text ""
            ]
        ]
    }


formatTime : Time.Zone -> Posix -> String
formatTime zone time =
    String.concat
        [ Time.toHour zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toSecond zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        ]



-- Subsctiption


subscriptions : Memory -> Sub Event
subscriptions _ =
    Time.every 1000 ReceiveTick



-- Procedure


procedures : () -> Block Memory Event
procedures () _ =
    [ sleep 2000
    , requestInitialTime
    , Procedure.await <|
        \event _ ->
            case event of
                ReceiveInitialTime ( zone, time ) ->
                    [ setPageView <|
                        PageHome
                            { zone = zone
                            , time = time
                            , showActionButton = False
                            }
                    ]

                _ ->
                    []
    , putLog "Asynchronous thread for clock..."
    , Procedure.async clockProcedures
    , modifyPageHome <| \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ modifyPageHome <| \home -> { home | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []
    , Procedure.sync
        [ sleepProcedures1
        , sleepProcedures2
        ]
    , putLog "All child threads are complete."
    , modifyPageHome <| \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ modifyPageHome <| \home -> { home | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []
    , Procedure.race
        [ sleepProcedures1
        , sleepProcedures2
        ]
    , putLog "One of the child threads is complete."

    -- Avoid to quit, so that clockProcedures does not end.
    , Procedure.await <| \_ _ -> []
    ]


clockProcedures : Block Memory Event
clockProcedures _ =
    [ Procedure.await <|
        \event _ ->
            case event of
                ReceiveTick time ->
                    [ modifyPageHome <|
                        \home ->
                            { home | time = time }
                    ]

                _ ->
                    []
    , Procedure.jump clockProcedures
    ]


sleepProcedures1 : Block Memory Event
sleepProcedures1 _ =
    [ putLog "Sleep 5 sec."
    , sleep 5000
    , putLog "Slept 5 sec."
    ]


sleepProcedures2 : Block Memory Event
sleepProcedures2 _ =
    [ putLog "Sleep 10 sec."
    , sleep 10000
    , putLog "Slept 10 sec."
    ]



-- -- Helper procedures


requestInitialTime : Procedure Memory Event
requestInitialTime =
    Procedure.push <|
        \_ ->
            Task.map2 (\zone time -> ( zone, time )) Time.here Time.now
                |> Task.perform ReceiveInitialTime


setPageView : PageView -> Procedure Memory Event
setPageView page =
    Procedure.modify <| \memory -> { memory | page = page }


putLog : String -> Procedure Memory Event
putLog log =
    Procedure.modify <| \memory -> { memory | log = memory.log ++ log ++ "\n" }


modifyPageHome : (PageHome_ -> PageHome_) -> Procedure Memory Event
modifyPageHome f =
    Procedure.modify <|
        \memory ->
            case memory.page of
                PageHome home ->
                    { memory | page = PageHome <| f home }

                _ ->
                    memory


sleep : Float -> Procedure Memory Event
sleep msec =
    Procedure.block <|
        \_ ->
            [ Procedure.push <|
                \_ ->
                    Process.sleep msec
                        |> Task.perform (\() -> WakeUp)
            , Procedure.await <|
                \event _ ->
                    case event of
                        WakeUp ->
                            [ Procedure.none
                            ]

                        _ ->
                            []
            ]
