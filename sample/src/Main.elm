module Main exposing (Global, Local, PageView, Shared, main)

import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Process
import Task
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Procedure as Procedure
import Time exposing (Posix)


main : Program () Shared Global Local
main =
    Browser.document
        { init = init
        , procedure = procedure
        , view = view
        , subscriptions = subscriptions
        }


{-| The memory state shared by all threads.
-}
type alias Shared =
    { log : String
    , page : PageView
    }


init : Shared
init =
    { log = ""
    , page = PageLoading
    }


{-| Global events
-}
type Global
    = ReceiveTick Posix
    | ClickActionButton


{-| Local events that only affect a specific thread.
-}
type Local
    = ReceiveInitialTime ( Time.Zone, Posix )
    | WakeUp



-- View


type PageView
    = PageLoading
    | PageHome PageHome_


view : Shared -> Document Global
view shared =
    case shared.page of
        PageLoading ->
            pageLoading

        PageHome home ->
            pageHome shared.log home


pageLoading : Document Global
pageLoading =
    { title = "Loading - Sample application"
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


pageHome : String -> PageHome_ -> Document Global
pageHome log home =
    { title = "Home - Sample application"
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
                    , style "height" "10em"
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


subscriptions : Shared -> Sub Global
subscriptions _ =
    Time.every 1000 ReceiveTick



-- Procedure


type alias Procedure =
    Procedure.Procedure Shared Global Local


procedure : () -> Procedure
procedure () =
    Procedure.batch
        [ sleep 3000
        , requestInitialTime
        , Procedure.await <|
            \local _ ->
                case local of
                    ReceiveInitialTime ( zone, time ) ->
                        Just <|
                            setPageView <|
                                PageHome
                                    { zone = zone
                                    , time = time
                                    , showActionButton = False
                                    }

                    _ ->
                        Nothing
        , putLog "Forking thread for clock..."
        , Procedure.fork <| \_ -> clockProcedure
        , modifyPageHome <| \home -> { home | showActionButton = True }
        , putLog """Press "Action" button bellow."""
        , Procedure.awaitGlobal <|
            \global _ ->
                case global of
                    ClickActionButton ->
                        Just <|
                            Procedure.batch
                                [ modifyPageHome <| \home -> { home | showActionButton = False }
                                , putLog """"Action" button has pressed."""
                                ]

                    _ ->
                        Nothing
        , Procedure.syncAll
            [ sleepProcedure1
            , sleepProcedure2
            ]
        , putLog "All child threads have completed."
        , Procedure.quit
        , putLog "(Unreachable)"
        ]


clockProcedure : Procedure
clockProcedure =
    Procedure.batch
        [ Procedure.awaitGlobal <|
            \global _ ->
                case global of
                    ReceiveTick time ->
                        Just <|
                            modifyPageHome <|
                                \home ->
                                    { home | time = time }

                    _ ->
                        Nothing
        , Procedure.fork <| \_ -> clockProcedure
        ]


sleepProcedure1 : Procedure
sleepProcedure1 =
    Procedure.batch
        [ putLog "Sleep 5 sec."
        , sleep 5000
        , putLog "Slept 5 sec."
        ]


sleepProcedure2 : Procedure
sleepProcedure2 =
    Procedure.batch
        [ putLog "Sleep 10 sec."
        , sleep 10000
        , putLog "Slept 10 sec."
        ]



-- -- Helper procedures


requestInitialTime : Procedure
requestInitialTime =
    Procedure.push <|
        \_ ->
            Task.map2 (\zone time -> ( zone, time )) Time.here Time.now
                |> Task.perform ReceiveInitialTime


setPageView : PageView -> Procedure
setPageView page =
    Procedure.modify <| \shared -> { shared | page = page }


putLog : String -> Procedure
putLog log =
    Procedure.modify <| \shared -> { shared | log = shared.log ++ log ++ "\n" }


modifyPageHome : (PageHome_ -> PageHome_) -> Procedure
modifyPageHome f =
    Procedure.modify <|
        \shared ->
            case shared.page of
                PageHome home ->
                    { shared | page = PageHome <| f home }

                _ ->
                    shared


sleep : Float -> Procedure
sleep msec =
    Procedure.batch
        [ Procedure.push <|
            \_ ->
                Process.sleep msec
                    |> Task.perform (\() -> WakeUp)
        , Procedure.await <|
            \local _ ->
                case local of
                    WakeUp ->
                        Just Procedure.none

                    _ ->
                        Nothing
        ]
