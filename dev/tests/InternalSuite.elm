module InternalSuite exposing (suite)

import Expect
import Internal exposing (Lifter, Procedure)
import Internal.ThreadId as ThreadId exposing (ThreadId)
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        mainThreadId =
            ThreadId.init

        sendEventThreadId =
            ThreadId.inc mainThreadId

        childThreadId =
            ThreadId.inc sendEventThreadId

        asyncInForkThreadId =
            ThreadId.inc childThreadId

        sample3ThreadId1 =
            ThreadId.inc asyncInForkThreadId

        forkInForkThreadId =
            ThreadId.inc sample3ThreadId1

        sample3ThreadId2 =
            ThreadId.inc forkInForkThreadId

        sample4ThreadId1 =
            ThreadId.inc sample3ThreadId2

        sample5ThreadId1 =
            ThreadId.inc sample4ThreadId1

        sample3ThreadId3 =
            ThreadId.inc sample5ThreadId1

        sample4ThreadId2 =
            ThreadId.inc sample3ThreadId3

        sample5ThreadId2 =
            ThreadId.inc sample4ThreadId2

        sample3ThreadId4 =
            ThreadId.inc sample5ThreadId2

        initialState =
            { log = []
            , childLog = []
            }

        thread =
            Internal.fromProcedure initialState (sampleProcedure mainThreadId)

        sendEvent =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                thread

        cleanup0 =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                sendEvent

        receiveThreadEvent =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 0))
                cleanup0

        ignoreUnconcernedThreadEvent =
            Internal.run
                (Internal.setTarget mainThreadId Unconcerned)
                cleanup0

        ignoreAnotherThreadEvent =
            Internal.run
                (Internal.setTarget childThreadId (Event2 0))
                cleanup0

        awaitAgainAfterUnconcernedThreadEvent =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 1))
                ignoreUnconcernedThreadEvent

        awaitAgainAfterAnotherThreadEvent =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 1))
                ignoreAnotherThreadEvent

        receiveNextThreadEvent =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 2))
                receiveThreadEvent

        cleanup =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                receiveNextThreadEvent

        receiveChildEvent =
            Internal.run
                (Internal.setTarget mainThreadId (ChildEvent <| ChildEvent1 10))
                cleanup

        receiveInheritedEvent =
            Internal.run
                (Internal.setTarget mainThreadId (GlobalEvent "inherited"))
                cleanup

        forkInFork =
            Internal.run
                (Internal.setTarget mainThreadId (GlobalEvent "cleanup"))
                receiveInheritedEvent

        globalToAllThread =
            Internal.run
                (Internal.setTarget mainThreadId <| GlobalEvent "global to all")
                forkInFork

        receiveOnlyInChildThreads =
            Internal.run
                (Internal.setTarget childThreadId <| ChildEvent <| ChildEvent2 "only in child threads")
                forkInFork

        syncThreads =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                globalToAllThread

        globalToSyncedThreads =
            Internal.run
                (Internal.setTarget mainThreadId <| GlobalEvent "globalToSyncedThreads")
                syncThreads

        receiveEvent2InLeftThread =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 3))
                globalToSyncedThreads

        forkInSyncAlive =
            Internal.run
                (Internal.setTarget mainThreadId (Event1 "forkInSyncAlive"))
                receiveEvent2InLeftThread

        raceThreads =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                forkInSyncAlive

        globalToRacedThreads =
            Internal.run
                (Internal.setTarget mainThreadId <| GlobalEvent "globalToRacedThreads")
                raceThreads

        ignoreEvent2InCancelledThread =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 13))
                globalToRacedThreads

        forkInRaceAlive =
            Internal.run
                (Internal.setTarget mainThreadId (Event1 "forkInRaceAlive"))
                ignoreEvent2InCancelledThread

        cleanup2 =
            Internal.run
                (Internal.setTarget mainThreadId Cleanup)
                forkInRaceAlive

        changeLogAfterModifyAndThen =
            Internal.run
                (Internal.setTarget mainThreadId (GlobalEvent "changeLog"))
                cleanup2

        checkPreviousLength =
            Internal.run
                (Internal.setTarget mainThreadId (Event2 30))
                changeLogAfterModifyAndThen

        forkedThreadAlives =
            Internal.run
                (Internal.setTarget mainThreadId (GlobalEvent "alives"))
                checkPreviousLength
    in
    describe "Test thread behaviour"
        [ test "A hack to avoid elm-review warnings" <|
            \_ ->
                Expect.true "true"
                    (case ChildCmd ChildCmd1 of
                        ChildCmd ChildCmd1 ->
                            True

                        _ ->
                            False
                    )
        , test "thread" <|
            \_ ->
                Expect.equal (stateOf thread)
                    { cmds = [ ( mainThreadId, Cmd1 ), ( mainThreadId, Cmd2 ) ]
                    , log =
                        [ log mainThreadId "Start a thread"
                        , log mainThreadId "Cmd1 has pushed"
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc mainThreadId
                    }
        , test "sendEvent" <|
            \_ ->
                Expect.equal (stateOf sendEvent)
                    { cmds = []
                    , log =
                        [ log mainThreadId <| "sendEvent"
                        , log mainThreadId <| "Received event from child thread: send from child."
                        , log sendEventThreadId <| "Propagated from parent: send from child."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "cleanup0" <|
            \_ ->
                Expect.equal (stateOf cleanup0)
                    { cmds = []
                    , log =
                        []
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "receiveThreadEvent" <|
            \_ ->
                Expect.equal (stateOf receiveThreadEvent)
                    { cmds = []
                    , log =
                        [ log mainThreadId "Received Event2 message: 0"
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "ignoreUnconcernedThreadEvent" <|
            \_ ->
                Expect.equal (stateOf ignoreUnconcernedThreadEvent)
                    { cmds = []
                    , log =
                        []
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "ignoreAnotherThreadEvent" <|
            \_ ->
                Expect.equal (stateOf ignoreAnotherThreadEvent)
                    { cmds = []
                    , log =
                        []
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "awaitAgainAfterUnconcernedThreadEvent" <|
            \_ ->
                Expect.equal (stateOf awaitAgainAfterUnconcernedThreadEvent)
                    { cmds = []
                    , log =
                        [ log mainThreadId "Received Event2 message: 1"
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "awaitAgainAfterAnotherThreadEvent" <|
            \_ ->
                Expect.equal (stateOf awaitAgainAfterAnotherThreadEvent)
                    { cmds = []
                    , log =
                        [ log mainThreadId "Received Event2 message: 1"
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sendEventThreadId
                    }
        , test "receiveNextThreadEvent" <|
            \_ ->
                Expect.equal (stateOf receiveNextThreadEvent)
                    { cmds =
                        [ ( mainThreadId, Cmd1 )
                        , ( childThreadId, ChildCmd ChildCmd1 )
                        ]
                    , log =
                        [ log mainThreadId "Received Event2 message: 0"
                        , log mainThreadId "This is evaluated immediately after await."
                        , log childThreadId <| startForkedThread childThreadId
                        ]
                    , childLog =
                        [ log childThreadId "Child log."
                        ]
                    , nextThreadId = ThreadId.inc childThreadId
                    }
        , test "cleanup" <|
            \_ ->
                Expect.equal (stateOf cleanup)
                    { cmds = []
                    , log = []
                    , childLog = []
                    , nextThreadId = ThreadId.inc childThreadId
                    }
        , test "receiveChildEvent" <|
            \_ ->
                Expect.equal (stateOf receiveChildEvent)
                    { cmds = []
                    , log =
                        [ log childThreadId "Received ChildEvent1 in forked thread: 10"
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc childThreadId
                    }
        , test "receiveInheritedEvent" <|
            \_ ->
                Expect.equal (stateOf receiveInheritedEvent)
                    { cmds = []
                    , log =
                        [ log mainThreadId "Received GlobalEvent in original thread: inherited"
                        , log childThreadId "Received InheritedEvent in forked thread: inherited"
                        , log asyncInForkThreadId "Evaluate asyncProcedure."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc asyncInForkThreadId
                    }
        , test "forkInFork" <|
            \_ ->
                Expect.equal (stateOf forkInFork)
                    { cmds = [ ( mainThreadId, Cmd2 ), ( childThreadId, ChildCmd ChildCmd2 ) ]
                    , log =
                        [ log sample3ThreadId1 <| evaluateSP3 sample3ThreadId1
                        , log asyncInForkThreadId "asyncProcedure received InheritedEvent: cleanup."
                        , log forkInForkThreadId "Evaluate sampleProcedure2."
                        , log asyncInForkThreadId "asyncProcedure is cancelled."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc forkInForkThreadId
                    }
        , test "globalToAllThread" <|
            \_ ->
                Expect.equal (stateOf globalToAllThread)
                    { cmds = []
                    , log =
                        [ log mainThreadId "original thread received GlobalEvent: global to all."
                        , log sample3ThreadId1 "sampleProcedure3 received GlobalEvent: global to all."
                        , log forkInForkThreadId "sampleProcedure2 received InheritedEvent: global to all."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc forkInForkThreadId
                    }
        , test "receiveOnlyInChildThreads" <|
            \_ ->
                Expect.equal (stateOf receiveOnlyInChildThreads)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId1 <| evaluateSP3 sample3ThreadId1
                        , log asyncInForkThreadId "asyncProcedure received InheritedEvent: cleanup."
                        , log forkInForkThreadId "Evaluate sampleProcedure2."
                        , log asyncInForkThreadId "asyncProcedure is cancelled."
                        , log forkInForkThreadId "sampleProcedure2 received ChildEvent2: only in child threads."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc forkInForkThreadId
                    }
        , test "syncThreads" <|
            \_ ->
                Expect.equal (stateOf syncThreads)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId2 <| evaluateSP3 sample3ThreadId2
                        , log sample4ThreadId1 "Evaluate sampleProcedure4."
                        , log sample5ThreadId1 "Evaluate sampleProcedure5."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId1
                    }
        , test "globalToSyncedThreads" <|
            \_ ->
                Expect.equal (stateOf globalToSyncedThreads)
                    { cmds = [ ( sample4ThreadId1, Cmd1 ) ]
                    , log =
                        [ log sample3ThreadId2 <| evaluateSP3 sample3ThreadId2
                        , log sample4ThreadId1 "Evaluate sampleProcedure4."
                        , log sample5ThreadId1 "Evaluate sampleProcedure5."
                        , log sample3ThreadId2 "sampleProcedure3 received GlobalEvent: globalToSyncedThreads."
                        , log sample4ThreadId1 "sampleProcedure4 received GlobalEvent: globalToSyncedThreads."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId1
                    }
        , test "receiveEvent2InLeftThread" <|
            \_ ->
                Expect.equal (stateOf receiveEvent2InLeftThread)
                    { cmds = [ ( sample4ThreadId1, Cmd2 ), ( mainThreadId, Cmd1 ) ]
                    , log =
                        [ log sample3ThreadId2 <| evaluateSP3 sample3ThreadId2
                        , log sample4ThreadId1 "Evaluate sampleProcedure4."
                        , log sample5ThreadId1 "Evaluate sampleProcedure5."
                        , log sample3ThreadId2 "sampleProcedure3 received GlobalEvent: globalToSyncedThreads."
                        , log sample4ThreadId1 "sampleProcedure4 received GlobalEvent: globalToSyncedThreads."
                        , log sample4ThreadId1 "sampleProcedure4 received Event2: 3."
                        , log sample4ThreadId1 "First finalizer for sampleProcedure4."
                        , log sample4ThreadId1 "Finalizer for sampleProcedure4."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId1
                    }
        , test "forkInSyncAlive" <|
            \_ ->
                Expect.equal (stateOf forkInSyncAlive)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId2 <| evaluateSP3 sample3ThreadId2
                        , log sample4ThreadId1 "Evaluate sampleProcedure4."
                        , log sample5ThreadId1 "Evaluate sampleProcedure5."
                        , log sample3ThreadId2 "sampleProcedure3 received GlobalEvent: globalToSyncedThreads."
                        , log sample4ThreadId1 "sampleProcedure4 received GlobalEvent: globalToSyncedThreads."
                        , log sample4ThreadId1 "sampleProcedure4 received Event2: 3."
                        , log sample4ThreadId1 "First finalizer for sampleProcedure4."
                        , log sample4ThreadId1 "Finalizer for sampleProcedure4."
                        , log sample5ThreadId1 "sampleProcedure5 received Event1: forkInSyncAlive."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId1
                    }
        , test "raceThreads" <|
            \_ ->
                Expect.equal (stateOf raceThreads)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId3 <| evaluateSP3 sample3ThreadId3
                        , log sample4ThreadId2 "Evaluate sampleProcedure4."
                        , log sample5ThreadId2 "Evaluate sampleProcedure5."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId2
                    }
        , test "globalToRacedThreads" <|
            \_ ->
                Expect.equal (stateOf globalToRacedThreads)
                    { cmds = [ ( sample4ThreadId2, Cmd1 ), ( mainThreadId, Cmd1 ) ]
                    , log =
                        [ log sample3ThreadId3 <| evaluateSP3 sample3ThreadId3
                        , log sample4ThreadId2 "Evaluate sampleProcedure4."
                        , log sample5ThreadId2 "Evaluate sampleProcedure5."
                        , log sample3ThreadId3 "sampleProcedure3 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "sampleProcedure4 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "First finalizer for sampleProcedure4."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId2
                    }
        , test "ignoreEvent2InCancelledThread" <|
            \_ ->
                Expect.equal (stateOf ignoreEvent2InCancelledThread)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId3 <| evaluateSP3 sample3ThreadId3
                        , log sample4ThreadId2 "Evaluate sampleProcedure4."
                        , log sample5ThreadId2 "Evaluate sampleProcedure5."
                        , log sample3ThreadId3 "sampleProcedure3 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "sampleProcedure4 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "First finalizer for sampleProcedure4."
                        , log sample4ThreadId2 "First finalizer for sampleProcedure4 received Event2."
                        , log sample4ThreadId2 "Finalizer in finalizer."
                        , log sample4ThreadId1 "First finalizer for sampleProcedure4 received Event2."
                        , log sample4ThreadId1 "Finalizer in finalizer."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId2
                    }
        , test "forkInRaceAlive" <|
            \_ ->
                Expect.equal (stateOf forkInRaceAlive)
                    { cmds = []
                    , log =
                        [ log sample3ThreadId3 <| evaluateSP3 sample3ThreadId3
                        , log sample4ThreadId2 "Evaluate sampleProcedure4."
                        , log sample5ThreadId2 "Evaluate sampleProcedure5."
                        , log sample3ThreadId3 "sampleProcedure3 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "sampleProcedure4 received GlobalEvent: globalToRacedThreads."
                        , log sample4ThreadId2 "First finalizer for sampleProcedure4."
                        , log sample4ThreadId2 "First finalizer for sampleProcedure4 received Event2."
                        , log sample4ThreadId2 "Finalizer in finalizer."
                        , log sample4ThreadId1 "First finalizer for sampleProcedure4 received Event2."
                        , log sample4ThreadId1 "Finalizer in finalizer."
                        , log sample5ThreadId2 "sampleProcedure5 received Event1: forkInRaceAlive."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample5ThreadId2
                    }
        , test "cleanup2" <|
            \_ ->
                Expect.equal (stateOf cleanup2)
                    { cmds = []
                    , log =
                        [ mat mainThreadId
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample3ThreadId4
                    }
        , test "changeLogAfterModifyAndThen" <|
            \_ ->
                Expect.equal (stateOf changeLogAfterModifyAndThen)
                    { cmds = []
                    , log =
                        [ mat mainThreadId
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        , log sample3ThreadId4 <| "sampleProcedure3 received GlobalEvent: changeLog."
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample3ThreadId4
                    }
        , test "checkPreviousLength" <|
            \_ ->
                Expect.equal (stateOf checkPreviousLength)
                    { cmds = []
                    , log =
                        [ mat mainThreadId
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        , log sample3ThreadId4 "sampleProcedure3 received GlobalEvent: changeLog."
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        , log mainThreadId <| previousLogLength 0
                        , log mainThreadId <| "Finalize sampleProcedure1."
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample3ThreadId4
                    }
        , test "forkedThreadAlives" <|
            \_ ->
                Expect.equal (stateOf forkedThreadAlives)
                    { cmds = []
                    , log =
                        [ mat mainThreadId
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        , log sample3ThreadId4 "sampleProcedure3 received GlobalEvent: changeLog."
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        , log mainThreadId <| previousLogLength 0
                        , log mainThreadId <| "Finalize sampleProcedure1."
                        , log sample3ThreadId4 "sampleProcedure3 received GlobalEvent: alives."
                        , log sample3ThreadId4 <| evaluateSP3 sample3ThreadId4
                        ]
                    , childLog = []
                    , nextThreadId = ThreadId.inc sample3ThreadId4
                    }
        ]



-- Helper functions


stateOf : Internal.Thread cmd Memory event -> { cmds : List ( ThreadId, cmd ), log : List String, childLog : List String, nextThreadId : ThreadId }
stateOf thread =
    let
        memory =
            Internal.peekMemory thread
    in
    { cmds = Internal.threadCmds thread
    , log = memory.log
    , childLog = memory.childLog
    , nextThreadId = Internal.nextThreadId thread
    }


log : ThreadId -> String -> String
log tid str =
    "thread " ++ ThreadId.toString tid ++ ": " ++ str


startForkedThread : ThreadId -> String
startForkedThread tid =
    "Start forked thread " ++ ThreadId.toString tid ++ "."


evaluateSP3 : ThreadId -> String
evaluateSP3 tid =
    "Evaluate sampleProcedure3 " ++ ThreadId.toString tid ++ "."


mat : ThreadId -> String
mat tid =
    "modifyAndThen " ++ ThreadId.toString tid


logLength : List String -> Int
logLength strs =
    List.map String.length strs
        |> List.sum


previousLogLength : Int -> String
previousLogLength n =
    "Previous log length: " ++ String.fromInt n



-- Memory


type alias Memory =
    { log : List String
    , childLog : List String
    }


type alias ChildMemory =
    { log : List String
    , parentLog : List String
    }


memoryLifter : Lifter Memory ChildMemory
memoryLifter =
    { get =
        \parent ->
            Just
                { log = parent.childLog
                , parentLog = parent.log
                }
    , set =
        \child parent ->
            { parent
                | childLog = child.log
                , log = child.parentLog
            }
    }


{-| Append a log message on the shared memory.
-}
putLog : String -> Procedure Cmd Memory Event
putLog str =
    Internal.modify <|
        \tid memory ->
            { memory
                | log = memory.log ++ [ log tid str ]
            }


{-| Append a child log message on the shared memory.
-}
putChildLog : String -> Procedure cmd ChildMemory event
putChildLog str =
    Internal.modify <|
        \tid memory ->
            { memory
                | log = memory.log ++ [ log tid str ]
            }


{-| Append a parent log message on the shared memory.
-}
putParentLog : String -> Procedure cmd ChildMemory event
putParentLog str =
    Internal.modify <|
        \tid memory ->
            { memory
                | parentLog = memory.parentLog ++ [ log tid str ]
            }


{-| Clear log messages saved on the shared memory.
-}
clearLog : Procedure Cmd Memory Event
clearLog =
    Internal.modify <|
        \_ _ ->
            { log = []
            , childLog = []
            }



-- Events


type Event
    = GlobalEvent String
    | Event1 String
    | Event2 Int
    | Unconcerned
    | Cleanup
    | ChildEvent ChildEvent


type ChildEvent
    = ChildEvent1 Int
    | ChildEvent2 String
    | InheritedEvent String


mgetChild : Event -> Maybe ChildEvent
mgetChild event =
    case event of
        ChildEvent c ->
            Just c

        GlobalEvent str ->
            Just <| InheritedEvent str

        _ ->
            Nothing


type Cmd
    = Cmd1
    | Cmd2
    | ChildCmd ChildCmd


type ChildCmd
    = ChildCmd1
    | ChildCmd2



-- Sample procedure


sampleProcedure : ThreadId -> Procedure Cmd Memory Event
sampleProcedure tid =
    Internal.batch
        [ putLog "Start a thread"
        , Internal.push <| \_ _ -> [ Cmd1 ]
        , putLog "Cmd1 has pushed"
        , Internal.push <| \_ _ -> [ Cmd2 ]
        , Internal.addFinalizer <| \_ -> putLog "Finalize sampleProcedure1."

        -- sendEvent
        , clear "sendEvent"
        , Internal.async <| sendEventProcedure tid
        , Internal.await <|
            \event _ ->
                case event of
                    Event1 str ->
                        Just <| putLog <| "Received event from child thread: " ++ str ++ "."

                    _ ->
                        Nothing

        -- cleanup0
        , Internal.await <|
            \event _ ->
                case event of
                    Cleanup ->
                        Just clearLog

                    _ ->
                        Nothing

        -- receiveThreadEvent,
        -- ignoreUnconcernedThreadEvent,
        -- ignoreAnotherThreadEvent,
        -- awaitAgainAfterUnconcernedThreadEvent,
        -- awaitAgainAfterAnotherThreadEvent,
        , Internal.await <|
            \event _ ->
                case event of
                    Event2 n ->
                        Just <|
                            Internal.batch
                                [ clearLog
                                , putLog <| "Received Event2 message: " ++ String.fromInt n
                                ]

                    _ ->
                        Nothing

        -- receiveNextThreadEvent,
        , Internal.await <|
            \event _ ->
                case event of
                    Event2 _ ->
                        Just Internal.none

                    -- Consumes msg, but do nothing
                    _ ->
                        Nothing
        , putLog "This is evaluated immediately after await."
        , Internal.fork <|
            \tid2 ->
                childProcedure tid2
                    |> Internal.liftMemory memoryLifter
                    |> Internal.liftEvent
                        { unwrap = mgetChild
                        , wrap = ChildEvent
                        }
                    |> Internal.mapCmd ChildCmd
        , Internal.push <| \_ _ -> [ Cmd1 ]

        -- cleanup
        , Internal.await <|
            \event _ ->
                case event of
                    Cleanup ->
                        Just clearLog

                    _ ->
                        Nothing

        -- receiveInheritedEvent
        , Internal.await <|
            \local _ ->
                case local of
                    GlobalEvent str ->
                        Just <| putLog <| "Received GlobalEvent in original thread: " ++ str

                    _ ->
                        Nothing

        -- forkInFork
        , Internal.await <|
            \event _ ->
                case event of
                    GlobalEvent _ ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.push <| \_ _ -> [ Cmd2 ]
        , Internal.fork <| \tid2 -> sampleProcedure3 tid2

        -- globalToAllThread
        , Internal.await <|
            \event _ ->
                case event of
                    GlobalEvent str ->
                        Just <|
                            Internal.batch
                                [ clearLog
                                , putLog <| "original thread received GlobalEvent: " ++ str ++ "."
                                ]

                    _ ->
                        Nothing

        -- syncThreads
        , Internal.await <|
            \event _ ->
                case event of
                    Cleanup ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.sync
            [ sampleProcedure3
            , \_ -> sampleProcedure4
            ]

        -- receiveEvent2InLeftThread
        , Internal.push <| \_ _ -> [ Cmd1 ]

        -- forkInSyncAlive
        -- raceThreads
        , Internal.await <|
            \event _ ->
                case event of
                    Cleanup ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.race
            [ sampleProcedure3
            , \_ -> sampleProcedure4
            ]
        , Internal.push <| \_ _ -> [ Cmd1 ]

        -- ignoreEvent2InCancelledThread
        -- forkInRaceAlive
        -- cleanup2
        , Internal.await <|
            \event _ ->
                case event of
                    Cleanup ->
                        Just <| clearLog

                    _ ->
                        Nothing
        , Internal.fork sampleProcedure3Infinite
        , Internal.modifyAndThen
            (\tid2 memory ->
                ( { memory
                    | log = [ mat tid2 ]
                  }
                , logLength memory.log
                )
            )
            (\_ n ->
                -- checkPreviousLength
                Internal.await <|
                    \event _ ->
                        case event of
                            Event2 _ ->
                                Just <| putLog <| previousLogLength n

                            _ ->
                                Nothing
            )
        , Internal.quit
        , putLog "Unreachable"
        ]


clear : String -> Procedure Cmd Memory Event
clear str =
    Internal.await <|
        \event _ ->
            case event of
                Cleanup ->
                    Just <|
                        Internal.batch
                            [ clearLog
                            , putLog str
                            ]

                _ ->
                    Nothing


childProcedure : ThreadId -> Procedure ChildCmd ChildMemory ChildEvent
childProcedure tid =
    Internal.batch
        -- receiveNextThreadEvent - childThreadId
        [ putParentLog <| startForkedThread tid
        , putChildLog <| "Child log."
        , Internal.push <| \_ _ -> [ ChildCmd1 ]

        -- receiveChildEvent,
        -- receiveInheritedEvent,
        , Internal.await <|
            \event _ ->
                case event of
                    ChildEvent1 n ->
                        Just <| putParentLog <| "Received ChildEvent1 in forked thread: " ++ String.fromInt n

                    InheritedEvent str ->
                        Just <|
                            Internal.batch
                                [ putParentLog <| "Received InheritedEvent in forked thread: " ++ str
                                , Internal.async <| \_ -> asyncProcedure
                                ]

                    _ ->
                        Nothing

        -- forkInFork
        , Internal.await <|
            \event _ ->
                case event of
                    InheritedEvent _ ->
                        Just <| Internal.none

                    _ ->
                        Nothing
        , Internal.push <| \_ _ -> [ ChildCmd2 ]
        , Internal.fork <| \_ -> sampleProcedure2
        ]


sampleProcedure2 : Procedure ChildCmd ChildMemory ChildEvent
sampleProcedure2 =
    Internal.batch
        -- forkInFork - forkInForkThreadId
        [ putParentLog "Evaluate sampleProcedure2."

        -- globalToAllThread
        -- receiveOnlyInChildThreads
        , Internal.await <|
            \event _ ->
                case event of
                    InheritedEvent str ->
                        Just <| putParentLog <| "sampleProcedure2 received InheritedEvent: " ++ str ++ "."

                    ChildEvent2 str ->
                        Just <| putParentLog <| "sampleProcedure2 received ChildEvent2: " ++ str ++ "."

                    _ ->
                        Nothing
        , Internal.quit
        , putChildLog "Unreachable in procedure2"
        ]


asyncProcedure : Procedure ChildCmd ChildMemory ChildEvent
asyncProcedure =
    Internal.batch
        -- receiveChildEvent, - asyncInForkThreadId
        -- receiveInheritedEvent, - asyncInForkThreadId
        [ putParentLog "Evaluate asyncProcedure."
        , Internal.addFinalizer <| \_ -> putParentLog "asyncProcedure is cancelled."

        -- forkInFork
        , Internal.await <|
            \event _ ->
                case event of
                    InheritedEvent str ->
                        Just <| putParentLog <| "asyncProcedure received InheritedEvent: " ++ str ++ "."

                    _ ->
                        Nothing
        , Internal.await <|
            \event _ ->
                case event of
                    InheritedEvent str ->
                        Just <| putParentLog <| "asyncProcedure received InheritedEvent again: " ++ str ++ "."

                    _ ->
                        Nothing
        ]


sendEventProcedure : ThreadId -> ThreadId -> Procedure Cmd Memory Event
sendEventProcedure parentTid _ =
    Internal.batch
        -- sendEvent
        [ Internal.send parentTid (Event1 "send from child")
        , Internal.await <|
            \event _ ->
                case event of
                    Event1 str ->
                        Just <| putLog <| "Propagated from parent: " ++ str ++ "."

                    _ ->
                        Nothing
        ]


sampleProcedure3 : ThreadId -> Procedure Cmd Memory Event
sampleProcedure3 tid =
    Internal.batch
        -- forkInFork - sample3ThreadId1
        --   syncThreads - sample3ThreadId2
        --     raceThreads - sample3ThreadId3
        --       cleanup2 - sample3ThreadId4
        [ putLog <| evaluateSP3 tid

        -- globalToAllThread
        --   globalToSyncedThreads
        --      globalToRacedThreads
        --        changeLogAfterModifyAndThen
        , Internal.await <|
            \event _ ->
                case event of
                    GlobalEvent str ->
                        Just <| putLog <| "sampleProcedure3 received GlobalEvent: " ++ str ++ "."

                    _ ->
                        Nothing
        ]


sampleProcedure4 : Procedure Cmd Memory Event
sampleProcedure4 =
    Internal.batch
        -- syncThreads - sample4ThreadId1
        --   raceThreads - sample4ThreadId2
        [ putLog "Evaluate sampleProcedure4."
        , Internal.fork sampleProcedure5
        , Internal.addFinalizer <|
            \_ ->
                Internal.batch
                    [ putLog <| "First finalizer for sampleProcedure4."
                    , Internal.addFinalizer <| \_ -> putLog "Finalizer in finalizer."
                    , Internal.await <|
                        \event _ ->
                            case event of
                                Event2 _ ->
                                    Just <| putLog <| "First finalizer for sampleProcedure4 received Event2."

                                _ ->
                                    Nothing
                    ]

        -- globalToSyncedThreads
        --   globalToRacedThreads
        , Internal.await <|
            \event _ ->
                case event of
                    GlobalEvent str ->
                        Just <| putLog <| "sampleProcedure4 received GlobalEvent: " ++ str ++ "."

                    _ ->
                        Nothing
        , Internal.push <| \_ _ -> [ Cmd1 ]

        -- receiveEvent2InLeftThread
        --   ignoreEvent2InCancelledThread
        , Internal.await <|
            \event _ ->
                case event of
                    Event2 n ->
                        Just <| putLog <| "sampleProcedure4 received Event2: " ++ String.fromInt n ++ "."

                    _ ->
                        Nothing
        , Internal.addFinalizer <| \_ -> putLog "Finalizer for sampleProcedure4."
        , Internal.push <| \_ _ -> [ Cmd2 ]
        ]


sampleProcedure5 : ThreadId -> Procedure Cmd Memory Event
sampleProcedure5 _ =
    Internal.batch
        -- syncThreads
        --   raceThreads
        [ putLog <| "Evaluate sampleProcedure5."

        -- forkInSyncAlive
        --   forkInRaceAlive
        , Internal.await <|
            \event _ ->
                case event of
                    Event1 str ->
                        Just <| putLog <| "sampleProcedure5 received Event1: " ++ str ++ "."

                    _ ->
                        Nothing
        ]


sampleProcedure3Infinite : ThreadId -> Procedure Cmd Memory Event
sampleProcedure3Infinite tid =
    Internal.batch
        [ sampleProcedure3 tid
        , Internal.addFinalizer <| \_ -> putLog "Never called"
        , Internal.jump (\_ -> sampleProcedure3Infinite tid)
        ]
