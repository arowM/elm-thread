module Internal exposing
    ( Thread
    , fromProcedure
    , cue
    , runWithMsg
    , ThreadState
    , initialState
    , ThreadResult
    , Msg
    , globalEvent
    , threadEvent
    , Procedure
    , none
    , batch
    , modify
    , push
    , await
    , awaitGlobal
    , fork
    , syncAll
    , quit
    )

{-|


# Core

@docs Thread
@docs fromProcedure
@docs cue
@docs runWithMsg
@docs ThreadState
@docs initialState
@docs ThreadResult
@docs Msg
@docs globalEvent
@docs threadEvent


# Procedure

@docs Procedure
@docs none
@docs batch
@docs modify
@docs push
@docs await
@docs awaitGlobal
@docs fork
@docs syncAll
@docs quit

-}

import Internal.ThreadId as ThreadId exposing (ThreadId)


{-| Thread processing `Procedure`.

  - localCmd: Side effects on processing the procedure
      - In a real use case, `Cmd local` is set here, but this module implicitly specify this type argument for testing.
  - shared: Shared memory
  - global: Global events
  - local: Local events that only affect the thread itself

-}
type Thread localCmd shared global local
    = EndOfThread
    | WithoutMsg (ThreadState shared -> ThreadResult localCmd shared global local)
    | WithMsg (Msg global local -> ThreadState shared -> ThreadResult localCmd shared global local)


{-| State to evaluate a thread.
-}
type alias ThreadState shared =
    { shared : shared
    , nextThreadId : ThreadId
    }


{-| -}
initialState : shared -> ThreadState shared
initialState shared =
    { shared = shared
    , nextThreadId = ThreadId.inc ThreadId.init
    }


{-| Result of thread evaluation.

  - newState: New thread state after the evaluation.
  - next: New thread to evaluate next time.
  - cmds: Side effects caused by the evaluation.

-}
type alias ThreadResult localCmd shared global local =
    { cmds : List ( ThreadId, localCmd )
    , newState : ThreadState shared
    , next : Thread localCmd shared global local
    }


runInForkedThread : Thread localCmd shared global local -> Thread localCmd shared global local -> Thread localCmd shared global local
runInForkedThread t1 t2 =
    case ( t1, t2 ) of
        ( EndOfThread, _ ) ->
            t2

        ( _, EndOfThread ) ->
            t1

        ( WithoutMsg f1, _ ) ->
            WithoutMsg <|
                \a ->
                    let
                        res1 =
                            f1 a
                    in
                    { res1
                        | next = runInForkedThread res1.next t2
                    }

        ( _, WithoutMsg f2 ) ->
            WithoutMsg <|
                \a ->
                    let
                        res2 =
                            f2 a
                    in
                    { res2
                        | next = runInForkedThread t1 res2.next
                    }

        ( WithMsg f1, WithMsg f2 ) ->
            WithMsg <|
                \msg a ->
                    let
                        res1 =
                            f1 msg a

                        res2 =
                            f2 msg res1.newState
                    in
                    { res2
                        | cmds = res1.cmds ++ res2.cmds
                        , next = runInForkedThread res1.next res2.next
                    }


runInSameThread : Thread localCmd shared global local -> Thread localCmd shared global local -> Thread localCmd shared global local
runInSameThread t1 t2 =
    case ( t1, t2 ) of
        ( EndOfThread, _ ) ->
            t2

        ( _, EndOfThread ) ->
            t1

        ( WithoutMsg f1, _ ) ->
            WithoutMsg <|
                \a ->
                    let
                        res1 =
                            f1 a
                    in
                    { res1
                        | next = runInSameThread res1.next t2
                    }

        ( WithMsg f1, _ ) ->
            WithMsg <|
                \msg a ->
                    let
                        res1 =
                            f1 msg a
                    in
                    { res1
                        | next = runInSameThread res1.next t2
                    }


{-| -}
fromProcedure : Procedure localCmd shared global local -> Thread localCmd shared global local
fromProcedure (Procedure ps) =
    fromProcedure_ ThreadId.init ps


fromProcedure_ : ThreadId -> List (Procedure_ localCmd shared global local) -> Thread localCmd shared global local
fromProcedure_ myThreadId procs =
    case procs of
        [] ->
            EndOfThread

        (Do f) :: ps ->
            WithoutMsg <|
                \prevState ->
                    let
                        ( shared1, cmds1 ) =
                            f prevState.shared
                    in
                    { cmds =
                        cmds1
                            |> List.map (\cmd -> ( myThreadId, cmd ))
                    , newState = { prevState | shared = shared1 }
                    , next = fromProcedure_ myThreadId ps
                    }

        (Await f) :: ps2 ->
            WithMsg <|
                \msg prevState ->
                    case f myThreadId msg prevState.shared of
                        Nothing ->
                            { cmds = []
                            , newState = prevState
                            , next = fromProcedure_ myThreadId procs
                            }

                        Just (Procedure ps1) ->
                            { cmds = []
                            , newState = prevState
                            , next =
                                runInSameThread
                                    (fromProcedure_ myThreadId ps1)
                                    (fromProcedure_ myThreadId ps2)
                            }

        (Fork f) :: ps1 ->
            WithoutMsg <|
                \prevState ->
                    let
                        ps2 =
                            f ()

                        forkedThreadId =
                            prevState.nextThreadId

                        forked =
                            fromProcedure_ forkedThreadId ps2

                        newState =
                            { prevState | nextThreadId = ThreadId.inc prevState.nextThreadId }

                        res =
                            cue newState forked
                    in
                    { res
                        | next =
                            runInForkedThread
                                (fromProcedure_ myThreadId ps1)
                                res.next
                    }

        (SyncAll pss) :: ps2 ->
            WithoutMsg <|
                \prevState ->
                    let
                        forked =
                            List.foldl
                                (\ps acc ->
                                    let
                                        forkedThreadId =
                                            acc.nextThreadId
                                    in
                                    { nextThreadId = ThreadId.inc acc.nextThreadId
                                    , next =
                                        runInForkedThread
                                            acc.next
                                            (fromProcedure_ forkedThreadId ps)
                                    }
                                )
                                { nextThreadId = prevState.nextThreadId
                                , next = EndOfThread
                                }
                                pss
                    in
                    { cmds = []
                    , newState = { prevState | nextThreadId = forked.nextThreadId }
                    , next =
                        runInSameThread
                            forked.next
                            (fromProcedure_ myThreadId ps2)
                    }

        Quit :: _ ->
            EndOfThread


{-| -}
cue : ThreadState shared -> Thread localCmd shared global local -> ThreadResult localCmd shared global local
cue state thread =
    case thread of
        EndOfThread ->
            { cmds = []
            , newState = state
            , next = EndOfThread
            }

        WithoutMsg f ->
            let
                res1 =
                    f state

                res2 =
                    cue res1.newState res1.next
            in
            { res2
                | cmds = res1.cmds ++ res2.cmds
            }

        WithMsg _ ->
            { cmds = []
            , newState = state
            , next = thread
            }


{-| -}
runWithMsg : Msg global local -> ThreadState shared -> Thread localCmd shared global local -> ThreadResult localCmd shared global local
runWithMsg msg state thread =
    let
        res =
            cue state thread
    in
    case res.next of
        EndOfThread ->
            res

        WithoutMsg _ ->
            -- Unreachable branch
            let
                res2 =
                    cue res.newState res.next
            in
            { res2
                | cmds = res.cmds ++ res2.cmds
            }

        WithMsg f ->
            let
                res2 =
                    f msg res.newState

                res3 =
                    cue res2.newState res2.next
            in
            { res3
                | cmds = res.cmds ++ res2.cmds ++ res3.cmds
            }


{-| -}
type Msg global local
    = GlobalEvent global
    | ThreadEvent ThreadId local


{-| -}
globalEvent : global -> Msg global local
globalEvent =
    GlobalEvent


{-| -}
threadEvent : ThreadId -> local -> Msg global local
threadEvent =
    ThreadEvent


{-| Procedures to be processed in a thread.

  - localCmd: Side effects on processing the procedure
      - In a real use case, `Cmd local` is set here, but this module implicitly specify this type argument for testing.
  - shared: Shared memory
  - global: Global events
  - local: Local events that only affect the thread itself

-}
type Procedure localCmd shared global local
    = Procedure (List (Procedure_ localCmd shared global local))


{-| -}
none : Procedure localCmd shared global local
none =
    Procedure []


{-| -}
batch : List (Procedure localCmd shared global local) -> Procedure localCmd shared global local
batch procs =
    List.concatMap (\(Procedure ps) -> ps) procs
        |> Procedure


type Procedure_ localCmd shared global local
    = Do (shared -> ( shared, List localCmd ))
    | Await (ThreadId -> Msg global local -> shared -> Maybe (Procedure localCmd shared global local))
    | Fork (() -> List (Procedure_ localCmd shared global local))
    | SyncAll (List (List (Procedure_ localCmd shared global local)))
    | Quit


{-| -}
modify : (shared -> shared) -> Procedure localCmd shared global local
modify f =
    Procedure
        [ Do <| \shared -> ( f shared, [] )
        ]


{-| -}
push : (shared -> List localCmd) -> Procedure localCmd shared global local
push f =
    Procedure
        [ Do <| \shared -> ( shared, f shared )
        ]


{-| -}
await : (local -> shared -> Maybe (Procedure localCmd shared global local)) -> Procedure localCmd shared global local
await f =
    Procedure
        [ Await <|
            \tid msg shared ->
                case msg of
                    GlobalEvent _ ->
                        Nothing

                    ThreadEvent tid_ local ->
                        if tid == tid_ then
                            f local shared

                        else
                            Nothing
        ]


{-| -}
awaitGlobal : (global -> shared -> Maybe (Procedure localCmd shared global local)) -> Procedure localCmd shared global local
awaitGlobal f =
    Procedure
        [ Await <|
            \_ msg shared ->
                case msg of
                    GlobalEvent global ->
                        f global shared

                    ThreadEvent _ _ ->
                        Nothing
        ]


{-| -}
fork : (() -> Procedure localCmd shared global local) -> Procedure localCmd shared global local
fork f =
    Procedure
        [ Fork <|
            \a ->
                f a
                    |> (\(Procedure ps) -> ps)
        ]


{-| -}
syncAll : List (Procedure localCmd shared global local) -> Procedure localCmd shared global local
syncAll procs =
    Procedure
        [ SyncAll <| List.map (\(Procedure ps) -> ps) procs
        ]


{-| -}
quit : Procedure localCmd shared global local
quit =
    Procedure [ Quit ]
