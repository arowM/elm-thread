# elm-thread

[![Build Status](https://travis-ci.com/arowM/elm-thread.svg?branch=main)](https://travis-ci.com/arowM/elm-thread)  
[Document](https://package.elm-lang.org/packages/arowM/elm-thread/latest/)  
[Live demo](https://arowM.github.io/elm-thread/index.html)  

Enables TEA to describe procedures as it is.

# A Quick Example

The following code is an excerpt from `sample/src/Main.elm`.


```elm
main : Program () Shared Global Local
main =
    document
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

# For developers

Check the `dev` directory for testing internal modules.

## Install deps

```sh
$ npm i
```

## Format files

```sh
$ npm run format
```

## Run doc server

```sh
$ npm run start
```

## Build sample app

```sh
$ npm run build:sample
```

## Test

```sh
$ npm test
```
