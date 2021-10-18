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
    , modifyAndThen
    , lazy
    , Lifter
    , liftShared
    , liftLocal
    , liftGlobal
    , mapLocalCmd
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
@docs modifyAndThen
@docs lazy


# Converters

@docs Lifter
@docs liftShared
@docs liftLocal
@docs liftGlobal
@docs mapLocalCmd

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
    | Async (Thread localCmd shared global local)


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

        ( Async t1_, _ ) ->
            Async <| runInForkedThread t1_ t2

        ( _, Async EndOfThread ) ->
            t1

        ( _, Async (Async t2_) ) ->
            runInForkedThread t1 (Async t2_)

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

        ( _, Async (WithoutMsg f2) ) ->
            WithoutMsg <|
                \a ->
                    let
                        res2 =
                            f2 a
                    in
                    { res2
                        | next = runInForkedThread t1 (Async res2.next)
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

        ( WithMsg f1, Async (WithMsg f2) ) ->
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
                        , next = runInForkedThread res1.next (Async res2.next)
                    }


runInSameThread : Thread localCmd shared global local -> Thread localCmd shared global local -> Thread localCmd shared global local
runInSameThread t1 t2 =
    case ( t1, t2 ) of
        ( EndOfThread, _ ) ->
            t2

        ( _, EndOfThread ) ->
            t1

        ( Async _, _ ) ->
            runInForkedThread t2 t1

        ( _, Async _ ) ->
            runInForkedThread t1 t2

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

        (DoAndThen f) :: ps2 ->
            WithoutMsg <|
                \prevState ->
                    let
                        ( shared1, cmds1, ps1 ) =
                            f () prevState.shared
                    in
                    { cmds =
                        cmds1
                            |> List.map (\cmd -> ( myThreadId, cmd ))
                    , newState = { prevState | shared = shared1 }
                    , next =
                        runInSameThread
                            (fromProcedure_ myThreadId ps1)
                            (fromProcedure_ myThreadId ps2)
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

                        Just ps1 ->
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
                                (Async res.next)
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

        Async t ->
            let
                res =
                    cue state t
            in
            { res
                | next = Async res.next
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

        Async _ ->
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
    = DoAndThen (() -> shared -> ( shared, List localCmd, List (Procedure_ localCmd shared global local) ))
    | Await (ThreadId -> Msg global local -> shared -> Maybe (List (Procedure_ localCmd shared global local)))
    | Fork (() -> List (Procedure_ localCmd shared global local))
    | SyncAll (List (List (Procedure_ localCmd shared global local)))
    | Quit


{-| Use to lift shared type.
-}
type alias Lifter a b =
    { get : a -> b
    , set : b -> a -> a
    }


{-| -}
liftShared : Lifter a b -> Procedure localCmd b global local -> Procedure localCmd a global local
liftShared lifter (Procedure ps) =
    Procedure <| List.map (liftShared_ lifter) ps


liftShared_ : Lifter a b -> Procedure_ localCmd b global local -> Procedure_ localCmd a global local
liftShared_ lifter pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \_ a ->
                    let
                        ( b, cmds, ps ) =
                            f () (lifter.get a)
                    in
                    ( lifter.set b a
                    , cmds
                    , List.map (liftShared_ lifter) ps
                    )

        Await f ->
            Await <|
                \tid msg a ->
                    f tid msg (lifter.get a)
                        |> Maybe.map (List.map (liftShared_ lifter))

        Fork f ->
            Fork <|
                \_ ->
                    f ()
                        |> List.map (liftShared_ lifter)

        SyncAll pss ->
            SyncAll <|
                List.map (List.map (liftShared_ lifter)) pss

        Quit ->
            Quit


{-| -}
liftLocal : (a -> Maybe b) -> Procedure localCmd shared global b -> Procedure localCmd shared global a
liftLocal mget (Procedure ps) =
    List.map (liftLocal_ mget) ps
        |> Procedure


{-| -}
liftLocal_ : (a -> Maybe b) -> Procedure_ localCmd shared global b -> Procedure_ localCmd shared global a
liftLocal_ mget pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \_ shared ->
                    let
                        ( shared2, cmds, ps ) =
                            f () shared
                    in
                    ( shared2
                    , cmds
                    , List.map (liftLocal_ mget) ps
                    )

        Await f ->
            Await <|
                \tid msg shared ->
                    mapMaybeLocal mget msg
                        |> Maybe.andThen
                            (\b ->
                                f tid b shared
                                    |> Maybe.map (List.map (liftLocal_ mget))
                            )

        Fork f ->
            Fork <|
                \_ ->
                    f ()
                        |> List.map (liftLocal_ mget)

        SyncAll pss ->
            SyncAll <|
                List.map (List.map (liftLocal_ mget)) pss

        Quit ->
            Quit


mapMaybeLocal : (a -> Maybe b) -> Msg global a -> Maybe (Msg global b)
mapMaybeLocal f msg =
    case msg of
        GlobalEvent global ->
            Just <| GlobalEvent global

        ThreadEvent tid a ->
            Maybe.map (ThreadEvent tid) (f a)


{-| -}
liftGlobal : (a -> Maybe b) -> Procedure localCmd shared b local -> Procedure localCmd shared a local
liftGlobal mget (Procedure ps) =
    List.map (liftGlobal_ mget) ps
        |> Procedure


{-| -}
liftGlobal_ : (a -> Maybe b) -> Procedure_ localCmd shared b local -> Procedure_ localCmd shared a local
liftGlobal_ mget pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \_ shared ->
                    let
                        ( shared2, cmds, ps ) =
                            f () shared
                    in
                    ( shared2
                    , cmds
                    , List.map (liftGlobal_ mget) ps
                    )

        Await f ->
            Await <|
                \tid msg shared ->
                    mapMaybeGlobal mget msg
                        |> Maybe.andThen
                            (\b ->
                                f tid b shared
                                    |> Maybe.map (List.map (liftGlobal_ mget))
                            )

        Fork f ->
            Fork <|
                \_ ->
                    f ()
                        |> List.map (liftGlobal_ mget)

        SyncAll pss ->
            SyncAll <|
                List.map (List.map (liftGlobal_ mget)) pss

        Quit ->
            Quit


mapMaybeGlobal : (a -> Maybe b) -> Msg a local -> Maybe (Msg b local)
mapMaybeGlobal f msg =
    case msg of
        GlobalEvent a ->
            Maybe.map GlobalEvent (f a)

        ThreadEvent tid local ->
            Just <| ThreadEvent tid local


{-| -}
mapLocalCmd : (a -> b) -> Procedure a shared global local -> Procedure b shared global local
mapLocalCmd set (Procedure ps) =
    List.map (mapLocalCmd_ set) ps
        |> Procedure


{-| -}
mapLocalCmd_ : (a -> b) -> Procedure_ a shared global local -> Procedure_ b shared global local
mapLocalCmd_ set pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \_ shared ->
                    let
                        ( shared2, cmds, ps ) =
                            f () shared
                    in
                    ( shared2
                    , List.map set cmds
                    , List.map (mapLocalCmd_ set) ps
                    )

        Await f ->
            Await <|
                \tid msg shared ->
                    f tid msg shared
                        |> Maybe.map (List.map (mapLocalCmd_ set))

        Fork f ->
            Fork <|
                \_ ->
                    f ()
                        |> List.map (mapLocalCmd_ set)

        SyncAll pss ->
            SyncAll <|
                List.map (List.map (mapLocalCmd_ set)) pss

        Quit ->
            Quit


{-| -}
modify : (shared -> shared) -> Procedure localCmd shared global local
modify f =
    Procedure
        [ DoAndThen <| \_ shared -> ( f shared, [], [] )
        ]


{-| -}
push : (shared -> List localCmd) -> Procedure localCmd shared global local
push f =
    Procedure
        [ DoAndThen <| \_ shared -> ( shared, f shared, [] )
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
                                |> Maybe.map
                                    (\(Procedure ps) -> ps)

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
                            |> Maybe.map
                                (\(Procedure ps) -> ps)

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


{-| -}
modifyAndThen : (shared -> ( shared, x )) -> (x -> Procedure localCmd shared global local) -> Procedure localCmd shared global local
modifyAndThen f g =
    Procedure
        [ DoAndThen <|
            \_ shared ->
                let
                    ( shared2, x ) =
                        f shared

                    (Procedure ps) =
                        g x
                in
                ( shared2, [], ps )
        ]


{-| -}
lazy : (() -> Procedure localCmd shared global local) -> Procedure localCmd shared global local
lazy f =
    Procedure
        [ DoAndThen <|
            \_ shared ->
                ( shared, [], f () |> (\(Procedure ps) -> ps) )
        ]
