# elm-thread

[![Build Status](https://travis-ci.com/arowM/elm-thread.svg?branch=main)](https://travis-ci.com/arowM/elm-thread)  
[Document](https://package.elm-lang.org/packages/arowM/elm-thread/latest/)  
[Live demo](https://arowm.github.io/elm-thread/)  
[Live demo (advanced)](https://arowm.github.io/elm-thread/advanced.html)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

Extend TEA so that chronological specifications can be translated verbatim into applications.

# What is this for?

With elm-thread, you can translate verbatim the specification of a UX-aware application into an implementation with the same look and feel.

In a UX-aware application, it is natural to write the specification in chronological order.
This is because application users make decisions about what to do next, based on their experience of their previous operations and the application's response to those operations.
However, conventional TEA is not suitable for implementing such specifications: Every time the user interacts with the screen, you have to check the model in the `update` function and try hard to analyze "what time series did the user follow" to choose the next process. A lot of bugs are introduced in this kind of transformation work. The bad news is that these bugs are about the behaviour of the application, so you have to suffer through complex and difficult UI testing.

With elm-thread, you can solve such drawbacks of TEA. As shown in the following example, it is possible to implement time series processing as it looks. What a magical library!

# Terms

The terms referred to in this document are defined as follows:

* Procedure: Definitions of the processes that the application will perform, in order.
* Thread: Computational resources on which the Procedure is evaluated.
* Memory: State shared between threads.
* Event: Message generated and received only within specific threads.

# A Quick Example

The following code is an excerpt from [`sample/src/Main.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src).


```elm
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Procedure as Procedure exposing (Block)


main : Program () Memory Event
main =
    Browser.document
        { init = init
        , procedures = procedures
        , view = Browser.globalDocument view
        , subscriptions =
            Browser.globalSubscriptions subscriptions
        }


-- Procedure


procedures : () -> Block Memory Event
procedures () _ =
    [ sleep 2000

-- Hey, you know?
-- In the conventional TEA, every time you do a sleep
-- operation, you're sent to another branch of `update`
-- function, where you have to check your model to know
-- "Where did I come from?".
-- What an annoying process!

-- With elm-thread, you just put the subsequent procedure
-- right below it.

    , requestInitialTime

-- How intuitive to be able to write the result of the
-- above request right underneath it!
-- Can I say one more amazing thing?
-- The result of the above request can only be received
-- in this thread and its child threads, and has no effect
-- on any other thread.

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

-- You can, of course, start and run another procedure in
-- a new independent thread.

    , Procedure.async clockProcedures

-- The above procedure is running as an independent thread,
-- so the following procedures will run concurrently without
-- waiting for them to finish.

    , modifyPageHome <|
        \home -> { home | showActionButton = True }
    , putLog """Press "Action" button bellow."""
    , Procedure.await <|
        \event _ ->
            case event of
                ClickActionButton ->
                    [ modifyPageHome <| \home ->
                        { home | showActionButton = False }
                    , putLog
                        """"Action" button has pressed."""
                    ]

                _ ->
                    []

-- Sometimes you want to synchronise your processes, don't
-- you?
-- Use `sync` to make sure that all procedures are completed
-- before moving on to the subsequent procedures.

    , Procedure.sync
        [ sleepProcedures1
        , sleepProcedures2
        ]
    , putLog "All child threads are complete."

-- Use `race` to make sure that at least one of the
-- procedures is completed before moving on to the subsequent
-- procedures.

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



-- Core


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
pageLoading = Debug.todo "See `sample/src/Main.elm`"


type alias PageHome_ =
    { time : Posix
    , zone : Time.Zone
    , showActionButton : Bool
    }


pageHome : String -> PageHome_ -> Document Event
pageHome = Debug.todo "See `sample/src/Main.elm`"



-- Subsctiption


subscriptions : Memory -> Sub Event
subscriptions _ =
    Time.every 1000 ReceiveTick
```

# SPA Example

The following code is an excerpt from [`sample/src/SPA.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src).

```elm
import SPA.Page.Home as Home
import SPA.Page.Users as Users
import Thread.Lifter exposing (Lifter)
import Thread.Procedure as Procedure exposing (Block)
import Thread.Wrapper exposing (Wrapper)



-- Memory


type alias Memory =
    { home : Home.Memory
    , users : Users.Memory
    }


init : Memory
init =
    { home = Home.init
    , users = Users.init
    }


homeLifter : Lifter Memory Home.Memory
homeLifter =
    { get = .home >> Just
    , set = \home shared -> { shared | home = home }
    }


usersLifter : Lifter Memory Users.Memory
usersLifter =
    { get = .users >> Just
    , set = \users shared -> { shared | users = users }
    }



-- Event


type Event
    = Event1
    | HomeEvent Home.Event
    | UsersEvent Users.Event


homeWrapper : Wrapper Event Home.Event
homeWrapper =
    { unwrap =
        \event ->
            case event of
                HomeEvent home ->
                    Just home

                _ ->
                    Nothing
    , wrap = HomeEvent
    }


usersWrapper : Wrapper Event Users.Event
usersWrapper =
    { unwrap =
        \event ->
            case event of
                UsersEvent users ->
                    Just users

                _ ->
                    Nothing
    , wrap = UsersEvent
    }



-- Procedure


procedures : Block Memory Event
procedures _ =
    [ Procedure.async
        (Home.procedures
            |> Procedure.liftBlock homeLifter
            |> Procedure.wrapBlock homeWrapper
        )
    , Procedure.async
        (Users.procedures
            |> Procedure.liftBlock usersLifter
            |> Procedure.wrapBlock usersWrapper
        )
    , Debug.todo "subsequent procedures..."
    ]
```
