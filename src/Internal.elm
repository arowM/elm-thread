module Internal exposing
    ( Thread
    , threadCmds
    , peekMemory
    , nextThreadId
    , fromProcedure
    , run
    , Msg
    , mapMsg
    , setTarget
    , Procedure
    , none
    , batch
    , modify
    , push
    , send
    , await
    , fork
    , async
    , sync
    , race
    , quit
    , modifyAndThen
    , addFinalizer
    , jump
    , Lifter
    , Wrapper
    , liftMemory
    , liftEvent
    , mapCmd
    )

{-|


# Core

@docs Thread
@docs threadCmds
@docs peekMemory
@docs nextThreadId
@docs fromProcedure
@docs run
@docs Msg
@docs mapMsg
@docs setTarget


# Procedure

@docs Procedure
@docs none
@docs batch
@docs modify
@docs push
@docs send
@docs await
@docs fork
@docs async
@docs sync
@docs race
@docs quit
@docs modifyAndThen
@docs addFinalizer
@docs jump


# Converters

@docs Lifter
@docs Wrapper
@docs liftMemory
@docs liftEvent
@docs mapCmd

-}

import Internal.ThreadId as ThreadId exposing (ThreadId)


{-| Thread representation.

  - newState: New thread state after the evaluation.
  - next: New thread to evaluate next time.
  - cmds: Side effects caused by the evaluation.

-}
type Thread cmd memory event
    = Thread
        { newState : ThreadState memory
        , next : Msg event -> ThreadState memory -> Thread cmd memory event
        , cmds : List ( ThreadId, cmd )
        }


pureThread : ThreadState memory -> Thread cmd memory event
pureThread state =
    Thread
        { newState = state
        , next = \_ -> pureThread
        , cmds = []
        }


{-| State to evaluate a thread.
-}
type alias ThreadState memory =
    { memory : memory
    , nextThreadId : ThreadId
    }


initialState : memory -> ThreadState memory
initialState memory =
    { memory = memory
    , nextThreadId = ThreadId.inc ThreadId.init
    }


{-| -}
threadCmds : Thread cmd memory event -> List ( ThreadId, cmd )
threadCmds (Thread res) =
    res.cmds


{-| -}
peekMemory : Thread cmd memory event -> memory
peekMemory (Thread res) =
    res.newState.memory


{-| -}
nextThreadId : Thread cmd memory event -> ThreadId
nextThreadId (Thread res) =
    res.newState.nextThreadId


{-| -}
run : Msg event -> Thread cmd memory event -> Thread cmd memory event
run msg (Thread t) =
    t.next msg t.newState


{-| -}
fromProcedure : memory -> Procedure cmd memory event -> Thread cmd memory event
fromProcedure initialMemory (Procedure ps) =
    fromProcedure_ ThreadId.init (initialState initialMemory) [] ps
        |> toThread


toThread : FromProcedure cmd memory event -> Thread cmd memory event
toThread fp =
    case fp of
        Running o ->
            Thread
                { newState = o.newState
                , cmds = o.cmds
                , next =
                    \msg s ->
                        o.next msg s
                            |> toThread
                }
                |> digForks o.forks
                |> sendMsgs o.msgs

        Finalizing o ->
            Thread
                { newState = o.newState
                , cmds = o.cmds
                , next = \_ -> pureThread
                }
                |> digForks o.forks

        Completed o ->
            Thread
                { newState = o.newState
                , cmds = o.cmds
                , next = \_ -> pureThread
                }


digForks : (ThreadState memory -> FromProcedure cmd memory event) -> Thread cmd memory event -> Thread cmd memory event
digForks f (Thread t) =
    case f t.newState of
        Running o ->
            digForks o.forks <|
                Thread
                    { newState = o.newState
                    , next =
                        \msg s ->
                            t.next msg s
                                |> mergeIndependently
                                    (o.next msg >> toThread)
                    , cmds = t.cmds ++ o.cmds
                    }

        Finalizing o ->
            digForks o.forks <|
                Thread
                    { newState = o.newState
                    , next = t.next
                    , cmds = t.cmds ++ o.cmds
                    }

        Completed o ->
            Thread
                { newState = o.newState
                , next = t.next
                , cmds = t.cmds ++ o.cmds
                }


sendMsgs : List (Msg event) -> Thread cmd memory event -> Thread cmd memory event
sendMsgs msgs t =
    List.foldl run t msgs


mergeIndependently : (ThreadState memory -> Thread cmd memory event) -> Thread cmd memory event -> Thread cmd memory event
mergeIndependently f (Thread t) =
    let
        (Thread t2) =
            f t.newState
    in
    Thread
        { newState = t2.newState
        , next =
            \msg s ->
                t.next msg s
                    |> mergeIndependently (t2.next msg)
        , cmds = t.cmds ++ t2.cmds
        }


{-| Intermediate type, which helps to handle operations that affects ancestor threads.
-}
type FromProcedure cmd memory event
    = Running
        { newState : ThreadState memory
        , cmds : List ( ThreadId, cmd )
        , msgs : List (Msg event) -- `Msg`s sent to ancestor threads.
        , next : Msg event -> ThreadState memory -> FromProcedure cmd memory event

        -- , finalizing : Msg event -> ThreadState memory -> FromProcedure cmd memory event
        , forks : ThreadState memory -> FromProcedure cmd memory event
        , finally : ThreadState memory -> FromProcedure cmd memory event
        }
    | Finalizing
        { newState : ThreadState memory
        , cmds : List ( ThreadId, cmd )
        , msgs : List (Msg event)

        -- , finalizing : Msg event -> ThreadState memory -> FromProcedure cmd memory event
        , forks : ThreadState memory -> FromProcedure cmd memory event
        }
    | Completed
        { newState : ThreadState memory
        , cmds : List ( ThreadId, cmd )
        , msgs : List (Msg event)
        }


noProcedure : ThreadState memory -> FromProcedure cmd memory event
noProcedure s =
    Completed
        { newState = s
        , cmds = []
        , msgs = []
        }


fromProcedure_ : ThreadId -> ThreadState memory -> List ThreadId -> List (Procedure_ cmd memory event) -> FromProcedure cmd memory event
fromProcedure_ myThreadId state parents procs =
    case procs of
        [] ->
            Completed
                { newState = state
                , cmds = []
                , msgs = []
                }

        (DoAndThen f) :: ps2 ->
            let
                ( memory1, cmds1, ps1 ) =
                    f myThreadId state.memory

                newState =
                    { state
                        | memory = memory1
                    }
            in
            fromProcedure_ myThreadId newState parents (ps1 ++ ps2)
                |> prependCmds (List.map (\c -> ( myThreadId, c )) cmds1)

        (Do f) :: ps2 ->
            let
                ( memory1, cmds1 ) =
                    f myThreadId state.memory

                newState =
                    { state
                        | memory = memory1
                    }
            in
            fromProcedure_ myThreadId newState parents ps2
                |> prependCmds (List.map (\c -> ( myThreadId, c )) cmds1)

        (AddFinalizer f) :: ps2 ->
            let
                ps1 =
                    f myThreadId
            in
            fromProcedure_ myThreadId state parents ps2
                |> applyFinally
                    (\s -> fromProcedure_ myThreadId s parents ps1)

        (Await f) :: ps2 ->
            Running
                { newState = state
                , next =
                    \msg s ->
                        case f (myThreadId :: parents) msg s.memory of
                            Just ps1 ->
                                fromProcedure_ myThreadId s parents (ps1 ++ ps2)

                            Nothing ->
                                fromProcedure_ myThreadId s parents procs
                , cmds = []
                , msgs = []
                , forks = noProcedure
                , finally = noProcedure
                }

        (Async p f) :: ps2 ->
            let
                asyncedThreadId =
                    state.nextThreadId

                newState =
                    { state | nextThreadId = ThreadId.inc state.nextThreadId }
            in
            fromProcedure_ asyncedThreadId newState (myThreadId :: parents) (p asyncedThreadId)
                |> andThen
                    (\s -> fromProcedure_ myThreadId s parents ps2)
                |> andAsync
                    (\s ->
                        fromProcedure_ asyncedThreadId s (myThreadId :: parents) (f asyncedThreadId)
                    )

        (Fork f) :: ps2 ->
            let
                forkedThreadId =
                    state.nextThreadId

                newState =
                    { state | nextThreadId = ThreadId.inc state.nextThreadId }

                forked s =
                    f forkedThreadId
                        |> fromProcedure_ forkedThreadId s (myThreadId :: parents)
            in
            fromProcedure_ myThreadId newState parents ps2
                |> andForks forked

        (Sync fs) :: ps2 ->
            fromProcDeps state (myThreadId :: parents) fs
                |> andThen (\s -> fromProcedure_ myThreadId s parents ps2)

        (Race fs) :: ps2 ->
            fromProcRaceDeps state (myThreadId :: parents) fs
                |> andThen (\s -> fromProcedure_ myThreadId s parents ps2)

        (Turn f) :: ps2 ->
            let
                ps1 =
                    f myThreadId
            in
            fromProcedure_ myThreadId state parents ps1

        (Issue msgs) :: ps ->
            fromProcedure_ myThreadId state parents ps
                |> prependMsgs msgs

        Quit :: _ ->
            noProcedure state


prependMsgs : List (Msg event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
prependMsgs msgs fp =
    case fp of
        Running o ->
            Running { o | msgs = msgs ++ o.msgs }

        Finalizing o ->
            Finalizing { o | msgs = msgs ++ o.msgs }

        Completed o ->
            Completed { o | msgs = msgs ++ o.msgs }


prependCmds : List ( ThreadId, cmd ) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
prependCmds cmds fp =
    case fp of
        Running o ->
            Running { o | cmds = cmds ++ o.cmds }

        Finalizing o ->
            Finalizing { o | cmds = cmds ++ o.cmds }

        Completed o ->
            Completed { o | cmds = cmds ++ o.cmds }


applyFinally : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
applyFinally f fp =
    case fp of
        Running o ->
            Running
                { o
                    | finally =
                        \s ->
                            f s |> andIndependently o.finally
                    , next =
                        \msg s ->
                            o.next msg s
                                |> applyFinally f
                }

        Finalizing o ->
            Finalizing
                { o
                    | forks =
                        \s ->
                            f s |> andIndependently o.forks
                }

        Completed o ->
            Finalizing
                { newState = o.newState
                , cmds = o.cmds
                , msgs = o.msgs
                , forks = f
                }


{-| Run a function after the given `FromProcedure` ends.
-}
andThen : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andThen f fp =
    case fp of
        Running o ->
            Running
                { o
                    | next =
                        \msg s ->
                            o.next msg s
                                |> andThen f
                }

        Finalizing o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o2.next
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        , finally = o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        }

                Completed o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o.forks
                        }

        Completed o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o2.next
                        , forks = o2.forks
                        , finally = o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o2.forks
                        }

                Completed o2 ->
                    Completed
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        }


{-| -}
andForks : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andForks forked fp =
    case fp of
        Running o ->
            Running
                { o
                    | forks =
                        \s ->
                            o.forks s
                                |> andIndependently forked
                }

        Finalizing o ->
            Finalizing
                { o
                    | forks =
                        \s ->
                            o.forks s
                                |> andIndependently forked
                }

        Completed o ->
            Finalizing
                { newState = o.newState
                , cmds = o.cmds
                , msgs = o.msgs
                , forks = forked
                }


andAsync : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andAsync f fp =
    case fp of
        Running o ->
            case f o.newState of
                Running oasync ->
                    Running
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , next =
                            \msg s ->
                                o.next msg s
                                    |> andAsync (oasync.next msg)
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently oasync.forks
                        , finally =
                            \s ->
                                o.finally s
                                    |> andIndependently oasync.finally
                        }

                Finalizing oasync ->
                    Running
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , next = o.next
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently oasync.forks
                        , finally = o.finally
                        }

                Completed oasync ->
                    Running
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , next = o.next
                        , forks = o.forks
                        , finally = o.finally
                        }

        Finalizing o ->
            case f o.newState of
                Running oasync ->
                    Finalizing
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently oasync.forks
                                    |> andIndependently oasync.finally
                        }

                Finalizing oasync ->
                    Finalizing
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently oasync.forks
                        }

                Completed oasync ->
                    Finalizing
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , forks = o.forks
                        }

        Completed o ->
            case f o.newState of
                Running oasync ->
                    Finalizing
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , forks =
                            \s ->
                                oasync.forks s
                                    |> andIndependently oasync.finally
                        }

                Finalizing oasync ->
                    Finalizing
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        , forks = oasync.forks
                        }

                Completed oasync ->
                    Completed
                        { newState = oasync.newState
                        , cmds = o.cmds ++ oasync.cmds
                        , msgs = o.msgs ++ oasync.msgs
                        }


andIndependently : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andIndependently =
    andForks


fromProcDeps : ThreadState memory -> List ThreadId -> List (ThreadId -> List (Procedure_ cmd memory event)) -> FromProcedure cmd memory event
fromProcDeps state parents fs =
    List.foldl
        (\f acc ->
            acc
                |> andNextDep
                    (\s ->
                        f s.nextThreadId
                            |> fromProcedure_
                                s.nextThreadId
                                { s | nextThreadId = ThreadId.inc s.nextThreadId }
                                parents
                    )
        )
        (noProcedure state)
        fs


andNextDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextDep f fp =
    case fp of
        Running o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next =
                            \msg s ->
                                o.next msg s
                                    |> andNextDep (o2.next msg)
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        , finally =
                            \s ->
                                o.finally s
                                    |> andIndependently o2.finally
                        }

                Finalizing o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o.next
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        , finally = o.finally
                        }

                Completed o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o.next
                        , forks = o.forks
                        , finally = o.finally
                        }

        Finalizing o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o2.next
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        , finally = o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        }

                Completed o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o.forks
                        }

        Completed o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next = o2.next
                        , forks = o2.forks
                        , finally = o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o2.forks
                        }

                Completed o2 ->
                    Completed
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        }


fromProcRaceDeps : ThreadState memory -> List ThreadId -> List (ThreadId -> List (Procedure_ cmd memory event)) -> FromProcedure cmd memory event
fromProcRaceDeps state parents fs =
    case fs of
        [] ->
            noProcedure state

        g :: gs ->
            List.foldl
                (\f acc ->
                    acc
                        |> andNextRaceDep
                            (\s ->
                                f s.nextThreadId
                                    |> fromProcedure_
                                        s.nextThreadId
                                        { s | nextThreadId = ThreadId.inc s.nextThreadId }
                                        parents
                            )
                )
                (g state.nextThreadId
                    |> fromProcedure_
                        state.nextThreadId
                        { state | nextThreadId = ThreadId.inc state.nextThreadId }
                        parents
                )
                gs


andNextRaceDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextRaceDep f acc =
    case acc of
        Running o ->
            case f o.newState of
                Running o2 ->
                    Running
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , next =
                            \msg s ->
                                o.next msg s
                                    |> andNextRaceDep (o2.next msg)
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        , finally =
                            \s ->
                                o.finally s
                                    |> andIndependently o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                                    |> andIndependently o.finally
                        }

                Completed o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o.finally
                        }

        Finalizing o ->
            case f o.newState of
                Running o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                                    |> andIndependently o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o.forks s
                                    |> andIndependently o2.forks
                        }

                Completed o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o.forks
                        }

        Completed o ->
            case f o.newState of
                Running o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks =
                            \s ->
                                o2.forks s
                                    |> andIndependently o2.finally
                        }

                Finalizing o2 ->
                    Finalizing
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        , forks = o2.forks
                        }

                Completed o2 ->
                    Completed
                        { newState = o2.newState
                        , cmds = o.cmds ++ o2.cmds
                        , msgs = o.msgs ++ o2.msgs
                        }


{-| -}
type Msg event
    = ThreadEvent ThreadId event


{-| -}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg f (ThreadEvent tid a) =
    ThreadEvent tid (f a)


{-| -}
setTarget : ThreadId -> event -> Msg event
setTarget =
    ThreadEvent


{-| Procedures to be processed in a thread.

  - cmd: Side effects on processing the procedure
      - In a real use case, `Cmd (Msg event)` is set here, but this module implicitly specify this type argument for testing.
  - memory: Shared memory
  - event: Message that only affect the certain threads

-}
type Procedure cmd memory event
    = Procedure (List (Procedure_ cmd memory event))


{-| -}
none : Procedure cmd memory event
none =
    Procedure []


{-| -}
batch : List (Procedure cmd memory event) -> Procedure cmd memory event
batch procs =
    List.concatMap (\(Procedure ps) -> ps) procs
        |> Procedure


type Procedure_ cmd memory event
    = DoAndThen (ThreadId -> memory -> ( memory, List cmd, List (Procedure_ cmd memory event) ))
    | Do (ThreadId -> memory -> ( memory, List cmd ))
    | AddFinalizer (ThreadId -> List (Procedure_ cmd memory event))
    | Await (List ThreadId -> Msg event -> memory -> Maybe (List (Procedure_ cmd memory event)))
      -- Run concurrently in new thread, alive even when parent thread ends
    | Fork (ThreadId -> List (Procedure_ cmd memory event))
      -- Run concurrently in new thread, killed when parent thread ends
    | Async
        (ThreadId -> List (Procedure_ cmd memory event))
        -- Preprocess
        (ThreadId -> List (Procedure_ cmd memory event))
    | Sync (List (ThreadId -> List (Procedure_ cmd memory event)))
    | Race (List (ThreadId -> List (Procedure_ cmd memory event)))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Turn (ThreadId -> List (Procedure_ cmd memory event))
    | Issue (List (Msg event))
    | Quit


{-| Use to lift memory type.
-}
type alias Lifter a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


{-| -}
liftMemory : Lifter a b -> Procedure cmd b event -> Procedure cmd a event
liftMemory lifter (Procedure ps) =
    Procedure <| List.map (liftMemory_ lifter) ps


liftMemory_ : Lifter a b -> Procedure_ cmd b event -> Procedure_ cmd a event
liftMemory_ lifter pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \tid a ->
                    case lifter.get a of
                        Nothing ->
                            ( a, [], [] )

                        Just old ->
                            let
                                ( b, cmds, ps ) =
                                    f tid old
                            in
                            ( lifter.set b a
                            , cmds
                            , List.map (liftMemory_ lifter) ps
                            )

        Do f ->
            Do <|
                \tid a ->
                    case lifter.get a of
                        Nothing ->
                            ( a, [] )

                        Just old ->
                            let
                                ( b, cmds ) =
                                    f tid old
                            in
                            ( lifter.set b a
                            , cmds
                            )

        AddFinalizer f ->
            AddFinalizer <|
                \tid ->
                    List.map (liftMemory_ lifter) (f tid)

        Await f ->
            Await <|
                \tids msg a ->
                    case lifter.get a of
                        Nothing ->
                            Nothing

                        Just old ->
                            f tids msg old
                                |> Maybe.map (List.map (liftMemory_ lifter))

        Fork f ->
            Fork <|
                \tid ->
                    f tid
                        |> List.map (liftMemory_ lifter)

        Async p f ->
            Async
                (\tid ->
                    p tid
                        |> List.map (liftMemory_ lifter)
                )
                (\tid ->
                    f tid
                        |> List.map (liftMemory_ lifter)
                )

        Sync fs ->
            Sync <|
                List.map (\f tid -> List.map (liftMemory_ lifter) (f tid)) fs

        Race fs ->
            Race <|
                List.map (\f tid -> List.map (liftMemory_ lifter) (f tid)) fs

        Turn f ->
            Turn <|
                \x ->
                    f x
                        |> List.map (liftMemory_ lifter)

        Issue msgs ->
            Issue msgs

        Quit ->
            Quit


{-| -}
liftEvent : Wrapper a b -> Procedure cmd memory b -> Procedure cmd memory a
liftEvent wrapper (Procedure ps) =
    List.map (liftEvent_ wrapper) ps
        |> Procedure


{-| Use to convert local event types.
-}
type alias Wrapper a b =
    { unwrap : a -> Maybe b
    , wrap : b -> a
    }


{-| -}
liftEvent_ : Wrapper a b -> Procedure_ cmd memory b -> Procedure_ cmd memory a
liftEvent_ wrapper pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \tid memory ->
                    let
                        ( memory2, cmds, ps ) =
                            f tid memory
                    in
                    ( memory2
                    , cmds
                    , List.map (liftEvent_ wrapper) ps
                    )

        Do f ->
            Do f

        AddFinalizer f ->
            AddFinalizer <|
                \tid ->
                    List.map (liftEvent_ wrapper) (f tid)

        Await f ->
            Await <|
                \tids msg memory ->
                    mapMaybeEvent wrapper.unwrap msg
                        |> Maybe.andThen
                            (\b ->
                                f tids b memory
                                    |> Maybe.map (List.map (liftEvent_ wrapper))
                            )

        Fork f ->
            Fork <|
                \tid ->
                    f tid
                        |> List.map (liftEvent_ wrapper)

        Async p f ->
            Async
                (\tid ->
                    p tid
                        |> List.map (liftEvent_ wrapper)
                )
                (\tid ->
                    f tid
                        |> List.map (liftEvent_ wrapper)
                )

        Sync fs ->
            Sync <|
                List.map (\f tid -> List.map (liftEvent_ wrapper) (f tid)) fs

        Race fs ->
            Race <|
                List.map (\f tid -> List.map (liftEvent_ wrapper) (f tid)) fs

        Turn f ->
            Turn <|
                \x ->
                    f x |> List.map (liftEvent_ wrapper)

        Issue msgs ->
            List.map
                (\(ThreadEvent tid a) -> ThreadEvent tid (wrapper.wrap a))
                msgs
                |> Issue

        Quit ->
            Quit


mapMaybeEvent : (a -> Maybe b) -> Msg a -> Maybe (Msg b)
mapMaybeEvent f msg =
    case msg of
        ThreadEvent tid a ->
            Maybe.map (ThreadEvent tid) (f a)


{-| -}
mapCmd : (a -> b) -> Procedure a memory event -> Procedure b memory event
mapCmd set (Procedure ps) =
    List.map (mapCmd_ set) ps
        |> Procedure


{-| -}
mapCmd_ : (a -> b) -> Procedure_ a memory event -> Procedure_ b memory event
mapCmd_ set pb =
    case pb of
        DoAndThen f ->
            DoAndThen <|
                \tid memory ->
                    let
                        ( memory2, cmds, ps ) =
                            f tid memory
                    in
                    ( memory2
                    , List.map set cmds
                    , List.map (mapCmd_ set) ps
                    )

        Do f ->
            Do <|
                \tid memory ->
                    let
                        ( memory2, cmds ) =
                            f tid memory
                    in
                    ( memory2
                    , List.map set cmds
                    )

        AddFinalizer f ->
            AddFinalizer <|
                \tid ->
                    List.map (mapCmd_ set) (f tid)

        Await f ->
            Await <|
                \tids msg memory ->
                    f tids msg memory
                        |> Maybe.map (List.map (mapCmd_ set))

        Fork f ->
            Fork <|
                \tid ->
                    f tid
                        |> List.map (mapCmd_ set)

        Async p f ->
            Async
                (\tid ->
                    p tid
                        |> List.map (mapCmd_ set)
                )
                (\tid ->
                    f tid
                        |> List.map (mapCmd_ set)
                )

        Sync fs ->
            Sync <|
                List.map (\f tid -> List.map (mapCmd_ set) (f tid)) fs

        Race fs ->
            Race <|
                List.map (\f tid -> List.map (mapCmd_ set) (f tid)) fs

        Turn f ->
            Turn <|
                \x ->
                    f x |> List.map (mapCmd_ set)

        Issue msgs ->
            Issue msgs

        Quit ->
            Quit


{-| Modifies the shared memory state.
The first argument takes current `ThreadId` and returns a modify function.
-}
modify : (ThreadId -> memory -> memory) -> Procedure cmd memory event
modify f =
    Procedure
        [ Do <| \tid memory -> ( f tid memory, [] )
        ]


{-| Push new `Cmd`.
The first argument takes current `ThreadId` and the shared memory state.
-}
push : (ThreadId -> memory -> List cmd) -> Procedure cmd memory event
push f =
    Procedure
        [ Do <| \tid memory -> ( memory, f tid memory )
        ]


{-| -}
await : (event -> memory -> Maybe (Procedure cmd memory event)) -> Procedure cmd memory event
await f =
    Procedure
        [ Await <|
            \tids msg memory ->
                case msg of
                    ThreadEvent tid event ->
                        if List.member tid tids then
                            f event memory
                                |> Maybe.map
                                    (\(Procedure ps) -> ps)

                        else
                            Nothing
        ]


{-| Run in independent thread, which alives even when the original thread ends.
The first argument takes current `ThreadId`.
-}
fork : (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
fork f =
    Procedure
        [ Fork <|
            \a ->
                f a
                    |> (\(Procedure ps) -> ps)
        ]


{-| Run in asynchronous thread, which ends when the original thread ends.
The first argument takes current `ThreadId`.
-}
async : (ThreadId -> Procedure cmd memory event) -> (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
async p f =
    Procedure
        [ Async
            (\a ->
                p a
                    |> (\(Procedure ps) -> ps)
            )
            (\a ->
                f a
                    |> (\(Procedure ps) -> ps)
            )
        ]


{-| Blocks thread till all the given threads are complete.
-}
sync : List (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
sync fs =
    Procedure
        [ Sync <|
            List.map
                (\f tid ->
                    let
                        (Procedure ps) =
                            f tid
                    in
                    ps
                )
                fs
        ]


{-| Blocks thread till one the given threads is complete.
It cancels any other given threads in progress.
-}
race : List (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
race fs =
    Procedure
        [ Race <|
            List.map
                (\f tid ->
                    let
                        (Procedure ps) =
                            f tid
                    in
                    ps
                )
                fs
        ]


{-| -}
send : ThreadId -> event -> Procedure cmd memory event
send tid event =
    Procedure [ Issue [ ThreadEvent tid event ] ]


{-| -}
quit : Procedure cmd memory event
quit =
    Procedure [ Quit ]


{-| -}
modifyAndThen : (ThreadId -> memory -> ( memory, x )) -> (ThreadId -> x -> Procedure cmd memory event) -> Procedure cmd memory event
modifyAndThen f g =
    Procedure
        [ DoAndThen <|
            \tid memory ->
                let
                    ( memory2, x ) =
                        f tid memory

                    (Procedure ps) =
                        g tid x
                in
                ( memory2, [], ps )
        ]


{-| -}
addFinalizer : (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
addFinalizer f =
    Procedure
        [ AddFinalizer <|
            \tid ->
                let
                    (Procedure ps) =
                        f tid
                in
                ps
        ]


{-| -}
jump : (ThreadId -> Procedure cmd memory event) -> Procedure cmd memory event
jump f =
    Procedure
        [ Turn <|
            \x ->
                let
                    (Procedure ps) =
                        f x
                in
                ps
        ]
