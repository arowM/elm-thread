module Thread.Procedure exposing
    ( Procedure
    , batch
    , ThreadId
    , Block
    , none
    , modify
    , push
    , await
    , async
    , asyncWith
    , block
    , sync
    , race
    , quit
    , jump
    , doUntil
    , addFinalizer
    , modifyAndThen
    , when
    , unless
    , withMaybe
    , withMemory
    , lift
    , Lifter
    , wrap
    , liftBlock
    , wrapBlock
    , init
    , initThreadId
    , update
    , Model
    , extractMemory
    , Msg
    , mapMsg
    , setTarget
    )

{-|


# Core

@docs Procedure
@docs batch
@docs ThreadId
@docs Block


# Constructors

@docs none
@docs modify
@docs push
@docs await
@docs async
@docs asyncWith
@docs block
@docs sync
@docs race
@docs quit
@docs jump
@docs doUntil
@docs addFinalizer
@docs modifyAndThen


# Conditions

@docs when
@docs unless
@docs withMaybe
@docs withMemory


# Converters

These items are needed when you try to build a hierarchy of memory and events in an SPA.
For a sample, see [`sample/src/SPA.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src).

These items are used to build memory and event hierarchies, for example in SPAs.
Note that the pattern often unnecessarily increases complexity, so you should first consider using monolithic shared memory and events.

@docs lift
@docs Lifter
@docs wrap
@docs liftBlock
@docs wrapBlock


# Lower level functions

It is recommended to use `Thread.Browser` for normal use.

@docs init
@docs initThreadId
@docs update
@docs Model
@docs extractMemory
@docs Msg
@docs mapMsg
@docs setTarget

-}

import Internal
import Internal.ThreadId as ThreadId
import Thread.Wrapper exposing (Wrapper)


{-| Procedures to be processed in a thread.

  - memory: State shared between threads.
  - event: Message generated and received only within specific threads.

-}
type alias Procedure memory event =
    Internal.Procedure memory event


{-| Construct a `Procedure` instance that do nothing.
-}
none : Procedure memory event
none =
    Internal.none


{-| Batch `Procedure`s together. The elements are evaluated in order.
-}
batch : List (Procedure memory event) -> Procedure memory event
batch =
    Internal.batch


{-| Reexposing `Thread.ThreadId.ThreadId` for convenience.
-}
type alias ThreadId =
    ThreadId.ThreadId


{-| An alias for a bunch of `Procedure`s.
-}
type alias Block memory event =
    ThreadId -> List (Procedure memory event)


{-| Construct a `Procedure` instance that modifies shared memory state.

Note that the update operation, passed as the first argument, is performed atomically. It means the state of shared memory read by a particular thread with `modify` is not updated by another thread until it is updated by the thread.

-}
modify : (memory -> memory) -> Procedure memory event
modify f =
    Internal.modify <|
        \_ memory ->
            f memory


{-| Construct a `Procedure` instance that issues a `Cmd` directed to the thread on which this function is evaluated.
-}
push : (memory -> Cmd event) -> Procedure memory event
push f =
    Internal.push <|
        \tid memory ->
            f memory
                |> Cmd.map (Internal.setTarget tid)
                |> List.singleton


{-| Construct a `Procedure` instance that awaits the local events for the thread.

If it returns empty list, it awaits again.
Otherwise, it evaluates the given `Procedure`.

Note1: The shared memory state passed to the first argument function may become outdated during running the thread for the `Procedure` generated by that function, so it is safe to use this shared memory state only to determine whether to accept or miss events.

Note2: Technically, all the `modify` `push` `async` written before the `await` will be executed internally as a single operation. This avoids the situation where a local event triggered by a `push` occurs while processing tons of subsequent `modify`s and `push`s, thus ensuring that the `await` always can catch the local event caused by the previous `Procedure`s.

Note3: `push`s written before an `await` will not necessarily cause local events in the order written. For example, if the first `push` sends a request to the server and it fires a local event with its result, and the second `push` sleeps for 0.1 seconds and then returns a local event, the first local event can fire later if the server is slow to respond. To avoid this situation, after using one `push`, catch it with `await` and use the next `push`, or use `sync`.

-}
await : (event -> memory -> List (Procedure memory event)) -> Procedure memory event
await f =
    Internal.await <|
        \local memory ->
            case f local memory of
                [] ->
                    Nothing

                ps ->
                    Just <| Internal.batch ps


{-| Construct a `Procedure` instance that evaluates the given `Block` in the asynchronous thread.

The asynchronous thread is provided new `ThreadId` and runs independently of the original thread; therefore the subsequent `Procedure`s in the original thread are evaluated immediately, and the asynchronous thread is cancelled when the original thread ends.

Infinite recursion by giving itself as the argument to `async` is not recommended to prevent threads from overgrowing. Use `jump` if you want to create threads that never end.

-}
async : Block memory event -> Procedure memory event
async f =
    Internal.async
        (\_ -> Internal.none)
        (\tid ->
            f tid
                |> batch
        )


{-| Like `async` but also takes options.

  - preprocess: Bunch of `Procedure`s that immediately evaluated with asynced thread ID
    Convenient for managing resources by asynced thread ID

This is to be removed in public release.

-}
asyncWith : { preprocess : Block memory event } -> Block memory event -> Procedure memory event
asyncWith { preprocess } f =
    Internal.async
        (\tid ->
            preprocess tid
                |> batch
        )
        (\tid ->
            f tid
                |> batch
        )


{-| Construct a `Procedure` instance that wait for the given `Procedure` to be completed.

Given `Procedure` is evaluated in the independent threads with new `ThreadId`, but the subsequent `Procedure`s in the original thread are **not** evaluated immediately. For example, the following sleep function uses `block` to scope the `WakeUp` event so that it only affects the inside of the `sleep` function.

    import Process
    import Task
    import Thread.Procedure as Procedure exposing (Procedure)

    sleep : Float -> Procedure Memory Event
    sleep msec =
        Procedure.block <|
            \_ ->
                [ Procedure.push <|
                    \_ ->
                        Process.sleep msec
                            |> Task.perform (\() -> WakeUp)
                , Procedure.await <|
                    \event _ ->
                        case event of
                            WakeUp ->
                                [ Procedure.none
                                ]

                            _ ->
                                []
                ]

Infinite recursion by giving itself as the argument to `async` is not recommended to prevent threads from overgrowing. Use `jump` if you want to create threads that never end.

-}
block : Block memory event -> Procedure memory event
block f =
    sync [ f ]


{-| Construct a `Procedure` instance that wait for all the given `Block`s to be completed.

Each `Block` is evaluated in the independent threads with its own `ThreadId`, but the subsequent `Procedure`s in the original thread are **not** evaluated immediately, but wait for all the given `Block`s to be completed.

-}
sync : List (Block memory event) -> Procedure memory event
sync fs =
    List.map
        (\f tid ->
            batch <| f tid
        )
        fs
        |> Internal.sync


{-| Construct a `Procedure` instance that wait for one of the given `Block`s to be completed.

Each `Block` is evaluated in the independent thread with its own `ThreadId`, but the subsequent `Procedure`s in the original thread are **not** evaluated immediately, but wait for one of the given `Block`s to be completed.

Note1: If one of the threads exits, all other threads will be suspended after processing until the next `await`.

-}
race : List (Block memory event) -> Procedure memory event
race fs =
    List.map
        (\f tid ->
            batch <| f tid
        )
        fs
        |> Internal.race


{-| Quit the thread immediately.

Subsequent `Procedure`s are not evaluated and are discarded.

-}
quit : Procedure memory event
quit =
    Internal.quit


{-| For a thread running this `Procedure`, add a finalizer: `Procedure`s to be evaluated when the thread is terminated, such as when the last `Procedure` for the thread has finished to be evaluated, or when the thread is interrupted by `quit` or `race`, or the parent thread ends by such reasons.

Since `addFinally` **appends** the finalizer, it is especially important to note that if you use `addFinally` in a thread that self-recurses with `turn`, the finalizer will be executed as many times as it self-recurses.

-}
addFinalizer : Block memory event -> Procedure memory event
addFinalizer f =
    Internal.addFinalizer <|
        \tid ->
            batch <| f tid


{-| Modify the shared memory atomically, creating the intermediate value in the process, and pass the value to the another `Block` in the original thread.

The intermediate value is supposed to be the information of the certain resource at a particular time.

-}
modifyAndThen : (memory -> ( memory, x )) -> (x -> Block memory event) -> Procedure memory event
modifyAndThen f g =
    Internal.modifyAndThen (\_ memory -> f memory) <|
        \tid x ->
            g x tid
                |> batch


{-| Ignore subsequent `Procedure`s, and evaluate given `Block` in the current thread. It is convenient for following two situations.


## Make recursive Block

Calling itself in the `Block` will result in a compile error; The `jump` avoids it to makes the recursive `Block`.

    import Thread.Procedure as Procedure exposing (Block)
    import Time exposing (Posix)

    clockProcedures : Block Memory Event
    clockProcedures tid =
        [ Procedure.await <|
            \event _ ->
                case event of
                    ReceiveTick time ->
                        [ Procedure.modify <|
                            \memory ->
                                { memory | time = time }
                        ]

                    _ ->
                        []
        , Procedure.jump clockProcedures
        ]

You can use `block` or `async` for a similar purpose, but whereas they create new threads for the given `Block`; it causes threads overgrowing.


## Safe pruning

Sometimes you may want to handle errors as follows:

    unsafePruning : Block Memory Event
    unsafePruning tid =
        [ requestPosts
        , Procedure.await <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ handleError error tid
                            |> Procedure.batch
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify <|
                            \memory ->
                                { memory | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.block blockForNewPosts
        ]

It appears to be nice, but it does not work as intended. Actually, the above `Block` can evaluate the `blockForNewPosts` even after evaluating `handleError`. To avoid this, you can use `jump`:

    safePruning : Block Memory Event
    safePruning tid =
        [ requestPosts
        , Procedure.await <|
            \event _ ->
                case event of
                    ReceivePosts (Err error) ->
                        [ Procedure.jump <| handleError error
                        ]

                    ReceivePosts (Ok posts) ->
                        [ Procedure.modify <|
                            \memory ->
                                { memory | posts = posts }
                        ]

                    _ ->
                        []
        , Procedure.block blockForNewPosts
        ]

-}
jump : Block memory event -> Procedure memory event
jump f =
    Internal.jump <|
        \tid ->
            f tid
                |> batch


{-| Evaluate another `Block`, provided as a first argument, with new `ThreadId` until the second argument returns non-empty list.

For example, you could use it to define a function that executes the `Block` for appropreate SPA page until the URL changes:

    import Thread.Procedure as Procedure exposing (Block)
    import Url exposing (Url)

    pageController : Route -> Block Memory Event
    pageController route tid =
        [ Procedure.doUntil
            -- The thread for the `pageProcedures` will be killed
            -- when the URL canges.
            (pageProcedures route)
          <|
            \event _ ->
                case event of
                    UrlChanged url ->
                        [ Procedure.jump <| pageController (routeFromUrl url)
                        ]

                    _ ->
                        []
        ]

-}
doUntil : Block memory event -> (event -> memory -> List (Procedure memory event)) -> Procedure memory event
doUntil blocker handler =
    batch
        [ async <|
            \_ ->
                [ race
                    [ \tid -> blocker tid
                    , \_ ->
                        [ await <|
                            \event memory ->
                                case handler event memory of
                                    [] ->
                                        []

                                    _ ->
                                        [ none ]
                        ]
                    ]
                ]
        , await handler
        ]



-- Lower level functions


{-| -}
type Model memory event
    = Model (Model_ memory event)


type alias Model_ memory event =
    { thread : Internal.Thread (Cmd (Internal.Msg event)) memory event
    }


{-| -}
extractMemory : Model memory event -> memory
extractMemory (Model model) =
    Internal.peekMemory model.thread


{-| -}
init : memory -> Block memory event -> ( Model memory event, Cmd (Msg event) )
init memory f =
    let
        proc =
            f initThreadId |> batch

        thread : Internal.Thread (Cmd (Internal.Msg event)) memory event
        thread =
            Internal.fromProcedure memory proc
    in
    ( Model
        { thread = thread
        }
    , batchLocalCmds <| Internal.threadCmds thread
    )


{-| `ThreadId` for the initially loaded procedure.

This is an alias for `Thread.ThreadId.init`.

-}
initThreadId : ThreadId
initThreadId =
    ThreadId.init


{-| -}
type Msg event
    = Msg (Internal.Msg event)


{-| -}
mapMsg : (a -> b) -> Msg a -> Msg b
mapMsg f (Msg msg) =
    Msg <| Internal.mapMsg f msg


{-| Set the target thread for an event by its `ThreadId`.
-}
setTarget : ThreadId -> event -> Msg event
setTarget tid event =
    Msg (Internal.setTarget tid event)


{-| -}
update : Msg event -> Model memory event -> ( Model memory event, Cmd (Msg event) )
update (Msg msg) (Model model) =
    let
        res =
            Internal.run msg model.thread
    in
    ( Model
        { thread = res
        }
    , batchLocalCmds <| Internal.threadCmds res
    )


batchLocalCmds : List ( ThreadId.ThreadId, Cmd (Internal.Msg event) ) -> Cmd (Msg event)
batchLocalCmds cmds =
    cmds
        |> List.map (\( _, cmd ) -> Cmd.map Msg cmd)
        |> Cmd.batch



-- Conditions


{-| Select a `Block` to run by the current memory state.

Do not use the provided memory state in the `Block` in order to avoid using outdated memory state.

-}
withMemory : (memory -> Block memory event) -> Procedure memory event
withMemory =
    modifyAndThen (\memory -> ( memory, memory ))


{-| Evaluate given `Procedure`s only if the first argument returns `True` with current memory state, otherwise returns `none`.
-}
when : (memory -> Bool) -> List (Procedure memory event) -> Procedure memory event
when f ls =
    withMemory <|
        \memory ->
            if f memory then
                \_ -> ls

            else
                \_ -> []


{-| Evaluate given `Procedure`s only if the first argument returns `False` with current memory state, otherwise returns `none`.
-}
unless : (memory -> Bool) -> List (Procedure memory event) -> Procedure memory event
unless f =
    when (not << f)


{-| If the first argument returns `Just a`, the given `Procedure`s are evaluated with the inner value `a`; otherwise returns `none`.
-}
withMaybe : (memory -> Maybe a) -> (a -> List (Procedure memory event)) -> Procedure memory event
withMaybe f g =
    withMemory <|
        \memory _ ->
            case f memory of
                Nothing ->
                    []

                Just a ->
                    g a



-- Converters


{-| Use to convert shared memory types.
-}
type alias Lifter a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


{-| Lift the memory type of of the given `Procedure`.

Note1: This is a low level function. The `Thread.Lifter` module exposes convenient high level functions for you.

Note2: This function does not set up a dedicated memory for `b`, but simply makes it operate on the part of memory `a`; so the memory `b` is shared with other threads.
If you want to create a thread that allocates a dedicated memory area of type `b` for a given procedure, use functions in the [`Thread.LocalMemory` module](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-LocalMemory).

-}
lift : Lifter a b -> Procedure b event -> Procedure a event
lift lifter proc =
    Internal.liftMemory lifter proc


{-| `Block` version of `liftMemory`.
-}
liftBlock :
    Lifter a b
    -> Block b event
    -> Block a event
liftBlock lifter f tid =
    f tid
        |> List.map (lift lifter)


{-| Wrap the event type of the given `Procedure`.
-}
wrap : Wrapper a b -> Procedure memory b -> Procedure memory a
wrap wrapper proc =
    Internal.mapCmd (Cmd.map (Internal.mapMsg wrapper.wrap)) proc
        |> Internal.liftEvent wrapper


{-| Wrap the event type of the given `Block`.
-}
wrapBlock :
    Wrapper a b
    -> Block memory b
    -> Block memory a
wrapBlock wrapper f tid =
    f tid
        |> List.map (wrap wrapper)
