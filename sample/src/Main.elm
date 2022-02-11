module Main exposing (Event, Memory, PageView, main)

import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Procedure exposing (Document, Msg, Observer, Program, global)
import Procedure.ObserverId exposing (ObserverId)
import Process
import Task
import Time exposing (Posix)


main : Program () Memory Event
main =
    Procedure.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = subscriptions
        }


{-| The memory state shared by all threads.
-}
type alias Memory =
    { page : PageView
    , log : String
    }


init : Memory
init =
    { page = PageLoading
    , log = ""
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
    | PageHome ( ObserverId, PageHome_ )


view : Memory -> Document (Msg Event)
view memory =
    case memory.page of
        PageLoading ->
            pageLoadingView

        PageHome ( oid, home ) ->
            pageHomeView oid memory.log home


pageLoadingView : Document msg
pageLoadingView =
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


pageHomeView : ObserverId -> String -> PageHome_ -> Document (Msg Event)
pageHomeView oid log home =
    let
        toMsg =
            Procedure.issue oid
    in
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
                        , Events.onClick (toMsg ClickActionButton)
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


subscriptions : Memory -> Sub (Msg Event)
subscriptions _ =
    Time.every 1000 (Procedure.publish << ReceiveTick)



-- Procedure


type alias Procedure =
    Procedure.Procedure (Cmd (Msg Event)) Memory Event


procedures : () -> List Procedure
procedures () =
    [ sleep 2000
    , requestInitialTime
    , Procedure.await global <|
        \event _ ->
            case event of
                ReceiveInitialTime ( zone, time ) ->
                    [ setPage
                        { wrap = PageHome
                        , unwrap = unwrapPageHome
                        }
                        { zone = zone
                        , time = time
                        , showActionButton = False
                        }
                        pageHomeProcedures
                    ]

                _ ->
                    -- When returning empty list, `await` awaits events again.
                    []
    ]


pageHomeProcedures : Observer Memory PageHome_ -> List Procedure
pageHomeProcedures pageHome =
    [ putLog "Asynchronous thread for clock..."
    , Procedure.async <| clockProcedures pageHome
    , Procedure.modify pageHome <|
        \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await pageHome <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ Procedure.modify pageHome <|
                        \home -> { home | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []
    , Procedure.sync
        [ sleepProcedures1
            |> Procedure.batch
        , sleepProcedures2
            |> Procedure.batch
        ]
    , putLog "All child threads are complete."
    , Procedure.modify pageHome <| \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await pageHome <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ Procedure.modify pageHome <| \home -> { home | showActionButton = False }
                    , putLog """"Action" button has pressed."""
                    ]

                _ ->
                    []
    , Procedure.race
        [ sleepProcedures1
            |> Procedure.batch
        , sleepProcedures2
            |> Procedure.batch
        ]
    , putLog "One of the child threads is complete."
    ]


unwrapPageHome : PageView -> Maybe ( ObserverId, PageHome_ )
unwrapPageHome pv =
    case pv of
        PageHome a ->
            Just a

        _ ->
            Nothing


clockProcedures : Observer Memory PageHome_ -> List Procedure
clockProcedures pageHome =
    [ Procedure.await global <|
        \event _ ->
            case event of
                ReceiveTick time ->
                    [ Procedure.modify pageHome <|
                        \home ->
                            { home | time = time }
                    ]

                _ ->
                    []
    , Procedure.jump global <| \_ -> clockProcedures pageHome
    ]


sleepProcedures1 : List Procedure
sleepProcedures1 =
    [ putLog "Sleep 5 sec."
    , sleep 5000
    , putLog "Slept 5 sec."
    ]


sleepProcedures2 : List Procedure
sleepProcedures2 =
    [ putLog "Sleep 10 sec."
    , sleep 10000
    , putLog "Slept 10 sec."
    ]



-- -- Helper procedures


requestInitialTime : Procedure
requestInitialTime =
    Procedure.push global <|
        \_ _ ->
            Task.map2 (\zone time -> ( zone, time )) Time.here Time.now
                |> Task.perform (Procedure.publish << ReceiveInitialTime)


putLog : String -> Procedure
putLog log =
    Procedure.modify global <| \memory -> { memory | log = memory.log ++ log ++ "\n" }


sleep : Float -> Procedure
sleep msec =
    Procedure.protected global <|
        \priv ->
            [ Procedure.push priv <|
                \oid _ ->
                    Process.sleep msec
                        |> Task.perform (\() -> Procedure.issue oid WakeUp)
            , Procedure.await priv <|
                \event _ ->
                    case event of
                        WakeUp ->
                            -- Do nothing, but do not await the next event.
                            [ Procedure.none
                            ]

                        _ ->
                            -- Do nothing, and await the next event again.
                            []
            ]


setPage :
    { wrap : ( ObserverId, b ) -> PageView
    , unwrap : PageView -> Maybe ( ObserverId, b )
    }
    -> b
    -> (Observer Memory b -> List Procedure)
    -> Procedure
setPage =
    Procedure.setVariant
        (global
            |> Procedure.dig
                { get = .page
                , set = \p memory -> { memory | page = p }
                }
        )
