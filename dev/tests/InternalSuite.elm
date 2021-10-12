module InternalSuite exposing (suite)

import Expect
import Internal exposing (Procedure)
import Internal.ThreadId as ThreadId
import Test exposing (Test, describe, test)


suite : Test
suite =
    let
        thread =
            Internal.fromProcedure sampleProcedure

        initialState =
            Internal.initialState ""

        cued =
            Internal.cue initialState thread

        mainThreadId =
            ThreadId.init

        forkedThreadId =
            ThreadId.inc mainThreadId

        forkedThreadId2 =
            ThreadId.inc forkedThreadId

        forkedThreadId3 =
            ThreadId.inc forkedThreadId2

        forkedThreadId4 =
            ThreadId.inc forkedThreadId3

        forkedThreadId5 =
            ThreadId.inc forkedThreadId4

        forkedThreadId6 =
            ThreadId.inc forkedThreadId5

        receiveThreadEvent =
            Internal.runWithMsg
                (Internal.threadEvent mainThreadId (Local1 "hi"))
                cued.newState
                cued.next

        ignoreAnotherThreadEvent =
            Internal.runWithMsg
                (Internal.threadEvent (ThreadId.inc mainThreadId) (Local1 "hi"))
                cued.newState
                cued.next

        ignoreGlobalEvent =
            Internal.runWithMsg
                (Internal.globalEvent (Global1 "HI"))
                cued.newState
                cued.next

        receiveGlobalEvent =
            Internal.runWithMsg
                (Internal.globalEvent (Global1 "HI"))
                receiveThreadEvent.newState
                receiveThreadEvent.next

        receiveAnotherGlobalEvent =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                receiveThreadEvent.newState
                receiveThreadEvent.next

        cleanup =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                receiveGlobalEvent.newState
                receiveGlobalEvent.next

        receiveForkedThreadEvent =
            Internal.runWithMsg
                (Internal.threadEvent forkedThreadId (Local2 10))
                cleanup.newState
                cleanup.next

        receiveMainThreadEvent =
            Internal.runWithMsg
                (Internal.threadEvent mainThreadId (Local2 20))
                cleanup.newState
                cleanup.next

        receiveGlobalEventOnForkedThread =
            Internal.runWithMsg
                (Internal.globalEvent (Global1 "FOO"))
                receiveForkedThreadEvent.newState
                receiveForkedThreadEvent.next

        receiveBothThreadEvent =
            Internal.runWithMsg
                (Internal.threadEvent forkedThreadId (Local2 10))
                receiveMainThreadEvent.newState
                receiveMainThreadEvent.next

        receiveGlobalEventOnBothThread =
            Internal.runWithMsg
                (Internal.globalEvent (Global1 "FOO"))
                receiveBothThreadEvent.newState
                receiveBothThreadEvent.next

        forkInFork =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                receiveGlobalEventOnBothThread.newState
                receiveGlobalEventOnBothThread.next

        globalToAllThread =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                forkInFork.newState
                forkInFork.next

        syncThreads =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                globalToAllThread.newState
                globalToAllThread.next

        globalToSyncedThreads =
            Internal.runWithMsg
                (Internal.globalEvent Global3)
                syncThreads.newState
                syncThreads.next

        globalToLeftThread =
            Internal.runWithMsg
                (Internal.globalEvent (Global1 "left"))
                globalToSyncedThreads.newState
                globalToSyncedThreads.next
    in
    describe "Test thread behaviour"
        [ test "cued" <|
            \_ ->
                Expect.equal
                    { cmds = cued.cmds
                    , newState = cued.newState
                    }
                    { cmds = [ ( mainThreadId, Cmd1 ), ( mainThreadId, Cmd2 ) ]
                    , newState =
                        { shared = """Start a thread
Cmd1 has pushed
"""
                        , nextThreadId = forkedThreadId
                        }
                    }
        , test "receiveThreadEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveThreadEvent.cmds
                    , newState = receiveThreadEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local1 message: hi
"""
                        , nextThreadId = forkedThreadId
                        }
                    }
        , test "ignoreAnotherThreadEvent" <|
            \_ ->
                Expect.equal
                    { cmds = ignoreAnotherThreadEvent.cmds
                    , newState = ignoreAnotherThreadEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Start a thread
Cmd1 has pushed
"""
                        , nextThreadId = forkedThreadId
                        }
                    }
        , test "ignoreGlobalEvent" <|
            \_ ->
                Expect.equal
                    { cmds = ignoreGlobalEvent.cmds
                    , newState = ignoreGlobalEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Start a thread
Cmd1 has pushed
"""
                        , nextThreadId = forkedThreadId
                        }
                    }
        , test "receiveGlobalEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveGlobalEvent.cmds
                    , newState = receiveGlobalEvent.newState
                    }
                    { cmds = [ ( mainThreadId, Cmd2 ), ( forkedThreadId, Cmd2 ), ( mainThreadId, Cmd1 ) ]
                    , newState =
                        { shared = """Received Global1 message: HI
This is evaluated immediately after await.
Start forked thread.
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveAnotherGlobalEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveAnotherGlobalEvent.cmds
                    , newState = receiveAnotherGlobalEvent.newState
                    }
                    { cmds = [ ( forkedThreadId, Cmd2 ), ( mainThreadId, Cmd1 ) ]
                    , newState =
                        { shared = """Received Local1 message: hi
This is evaluated immediately after await.
Start forked thread.
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveForkedThreadEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveForkedThreadEvent.cmds
                    , newState = receiveForkedThreadEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local2 in forked thread: 10
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveMainThreadEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveMainThreadEvent.cmds
                    , newState = receiveMainThreadEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local2 in original thread: 20
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveGlobalEventOnForkedThread" <|
            \_ ->
                Expect.equal
                    { cmds = receiveGlobalEventOnForkedThread.cmds
                    , newState = receiveGlobalEventOnForkedThread.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local2 in forked thread: 10
Received Global1 in forked thread: FOO
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveBothThreadEvent" <|
            \_ ->
                Expect.equal
                    { cmds = receiveBothThreadEvent.cmds
                    , newState = receiveBothThreadEvent.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local2 in original thread: 20
Received Local2 in forked thread: 10
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "receiveGlobalEventOnBothThread" <|
            \_ ->
                Expect.equal
                    { cmds = receiveGlobalEventOnBothThread.cmds
                    , newState = receiveGlobalEventOnBothThread.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Received Local2 in original thread: 20
Received Local2 in forked thread: 10
Received Global1 in original thread: FOO
Received Global1 in forked thread: FOO
"""
                        , nextThreadId = forkedThreadId2
                        }
                    }
        , test "forkInFork" <|
            \_ ->
                Expect.equal
                    { cmds = forkInFork.cmds
                    , newState = forkInFork.newState
                    }
                    { cmds = [ ( forkedThreadId, Cmd1 ) ]
                    , newState =
                        { shared = """Evaluate sampleProcedure3.
Evaluate sampleProcedure2.
"""
                        , nextThreadId = forkedThreadId4
                        }
                    }
        , test "globalToAllThread" <|
            \_ ->
                Expect.equal
                    { cmds = globalToAllThread.cmds
                    , newState = globalToAllThread.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """original thread
sampleProcedure3.
sampleProcedure2.
"""
                        , nextThreadId = forkedThreadId4
                        }
                    }
        , test "syncThreads" <|
            \_ ->
                Expect.equal
                    { cmds = syncThreads.cmds
                    , newState = syncThreads.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Evaluate sampleProcedure3.
Evaluate sampleProcedure4.
"""
                        , nextThreadId = forkedThreadId6
                        }
                    }
        , test "globalToSyncedThreads" <|
            \_ ->
                Expect.equal
                    { cmds = globalToSyncedThreads.cmds
                    , newState = globalToSyncedThreads.newState
                    }
                    { cmds = []
                    , newState =
                        { shared = """Evaluate sampleProcedure3.
Evaluate sampleProcedure4.
sampleProcedure3.
sampleProcedure4.
"""
                        , nextThreadId = forkedThreadId6
                        }
                    }
        , test "globalToLeftThread" <|
            \_ ->
                Expect.equal
                    { cmds = globalToLeftThread.cmds
                    , newState = globalToLeftThread.newState
                    }
                    { cmds = [ ( forkedThreadId5, Cmd2 ), ( mainThreadId, Cmd1 ) ]
                    , newState =
                        { shared = """Evaluate sampleProcedure3.
Evaluate sampleProcedure4.
sampleProcedure3.
sampleProcedure4.
sampleProcedure4: left
"""
                        , nextThreadId = forkedThreadId6
                        }
                    }
        ]



-- Shared memory


type alias Shared =
    String


{-| Append a log message on the shared memory.
-}
putLog : String -> Procedure LocalCmd Shared Global Local
putLog str =
    Internal.modify <| \shared -> shared ++ str ++ "\n"


{-| Clear log messages saved on the shared memory.
-}
clearLog : Procedure LocalCmd Shared Global Local
clearLog =
    Internal.modify <| \_ -> ""



-- Global events


type Global
    = Global1 String
    | Global3



-- Local events


type Local
    = Local1 String
    | Local2 Int


type LocalCmd
    = Cmd1
    | Cmd2



-- Sample procedure


sampleProcedure : Procedure LocalCmd Shared Global Local
sampleProcedure =
    Internal.batch
        [ putLog "Start a thread"
        , Internal.push <| \_ -> [ Cmd1 ]
        , putLog "Cmd1 has pushed"
        , Internal.push <| \_ -> [ Cmd2 ]

        -- Awaiting a thread event.
        , Internal.await <|
            \local _ ->
                case local of
                    Local1 str ->
                        Just <|
                            Internal.batch
                                [ clearLog
                                , putLog <| "Received Local1 message: " ++ str
                                ]

                    _ ->
                        Nothing

        -- Awaiting a global event.
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global1 str ->
                        Just <|
                            Internal.batch
                                [ clearLog
                                , Internal.push <| \_ -> [ Cmd2 ]
                                , putLog <| "Received Global1 message: " ++ str
                                ]

                    Global3 ->
                        -- Consumes msg, but do nothing
                        Just <| Internal.none
        , putLog "This is evaluated immediately after await."
        , Internal.fork <| \_ -> anotherProcedure
        , Internal.push <| \_ -> [ Cmd1 ]
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.await <|
            \local _ ->
                case local of
                    Local2 n ->
                        Just <| putLog <| "Received Local2 in original thread: " ++ String.fromInt n

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global1 str ->
                        Just <| putLog <| "Received Global1 in original thread: " ++ str

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.fork <| \_ -> sampleProcedure3
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just <|
                            Internal.batch
                                [ clearLog
                                , putLog <| "original thread"
                                ]

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just clearLog

                    _ ->
                        Nothing
        , Internal.syncAll
            [ sampleProcedure3
            , sampleProcedure4
            ]
        , Internal.push <| \_ -> [ Cmd1 ]
        , Internal.quit
        , putLog "Unreachable"
        ]


anotherProcedure : Procedure LocalCmd Shared Global Local
anotherProcedure =
    Internal.batch
        [ putLog "Start forked thread."
        , Internal.push <| \_ -> [ Cmd2 ]
        , Internal.await <|
            \local _ ->
                case local of
                    Local2 n ->
                        Just <| putLog <| "Received Local2 in forked thread: " ++ String.fromInt n

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global1 str ->
                        Just <| putLog <| "Received Global1 in forked thread: " ++ str

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just <| Internal.none

                    _ ->
                        Nothing
        , Internal.push <| \_ -> [ Cmd1 ]
        , Internal.fork <| \_ -> sampleProcedure2
        ]


sampleProcedure2 : Procedure LocalCmd Shared Global Local
sampleProcedure2 =
    Internal.batch
        [ putLog "Evaluate sampleProcedure2."
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just <| putLog <| "sampleProcedure2."

                    _ ->
                        Nothing
        ]


sampleProcedure3 : Procedure LocalCmd Shared Global Local
sampleProcedure3 =
    Internal.batch
        [ putLog "Evaluate sampleProcedure3."
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just <| putLog <| "sampleProcedure3."

                    _ ->
                        Nothing
        , Internal.quit
        , putLog "Unreachable in procedure3"
        ]


sampleProcedure4 : Procedure LocalCmd Shared Global Local
sampleProcedure4 =
    Internal.batch
        [ putLog "Evaluate sampleProcedure4."
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global3 ->
                        Just <| putLog <| "sampleProcedure4."

                    _ ->
                        Nothing
        , Internal.awaitGlobal <|
            \global _ ->
                case global of
                    Global1 str ->
                        Just <| putLog <| "sampleProcedure4: " ++ str

                    _ ->
                        Nothing
        , Internal.push <| \_ -> [ Cmd2 ]
        ]
