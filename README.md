# elm-thread

[![Build Status](https://travis-ci.com/arowM/elm-thread.svg?branch=main)](https://travis-ci.com/arowM/elm-thread)  
[Document](https://package.elm-lang.org/packages/arowM/elm-thread/latest/)  
[Live demo](https://arowm.github.io/elm-thread/)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

Extend TEA so that chronological specifications can be translated verbatim into applications.

# What is this for?

With elm-thread, you can translate _verbatim_ the specification of a UX-aware application into an implementation with the same look and feel.

In a UX-aware application, it is natural to write the specification in chronological order.
This is because application users make decisions about what to do next, based on their experience of their previous operations and the application's response to those operations.
However, conventional TEA is not suitable for implementing such specifications: Every time the user interacts with the screen, you have to check the model in the `update` function and try hard to analyze "what time series did the user follow" to choose the next process. A lot of bugs are introduced in this kind of transformation work. The bad news is that these bugs are about the behaviour of the application, so you have to suffer through complex and difficult UI testing.

With elm-thread, you can solve such drawbacks of TEA. As shown in the following example, it is possible to implement time series processing as it looks. What a magical library!

# A Quick Example

The following code is an excerpt from `sample/src/Main.elm`.


```elm
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Procedure as Procedure

main : Program () Shared Global Local
main =
    Browser.document
        { init = init
        , procedure = procedure
        , view = view
        , subscriptions = subscriptions
        }


-- Procedure


type alias Procedure =
    Procedure.Procedure Shared Global Local


procedure : () -> Procedure
procedure () =
    Procedure.batch
        [ sleep 3000

-- Hey, you know?
-- In the conventional TEA, every time you do a sleep operation,
-- you're sent to another branch of `update` function,
-- where you have to check your model to know "Where did I come from?".
-- What an annoying process!

-- With elm-thread, you just put the subsequent procedure right below it.

        , requestInitialTime

-- How intuitive to be able to write the result of the above request right underneath it!
-- Can I say one more amazing thing?
-- The result of the above request can only be received in this thread and has no effect on any other thread.

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

-- You can, of course, start and run another procedure as a thread independent of this one.

        , Procedure.fork <| \_ -> clockProcedure

-- The above procedure is running as an independent thread,
-- so the following procedures will run in parallel without waiting for them to finish.

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

-- Sometimes you want to synchronise your processes, don't you?
-- Use `syncAll` to make sure that all procedures are completed before moving on to the subsequent procedures.

        , Procedure.syncAll
            [ sleepProcedure1
            , sleepProcedure2
            ]

        , putLog "All child threads has completed."
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



-- Core


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


pageLoading = Debug.todo "See `sample/src/Main.elm`"


type alias PageHome_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


pageHome = Debug.todo "See `sample/src/Main.elm`"


-- Subsctiption


subscriptions : Shared -> Sub Global
subscriptions _ =
    Time.every 1000 ReceiveTick
```
