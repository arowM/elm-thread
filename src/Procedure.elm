module Procedure exposing
    ( element
    , document
    , application
    , Procedure
    , Program
    , Document
    , Model
    , Msg
    , issue
    , publish
    , none
    , batch
    , modify
    , push
    , await
    , async
    , sync
    , race
    , quit
    , jump
    , doUntil
    , protected
    , withResource
    , when
    , unless
    , withMaybe
    , Observer
    , global
    , dig
    , setVariant
    , prepend
    , append
    , insertBefore
    , insertAfter
    , mapCmd
    , memoryState
    , update
    , init
    )

{-|


# Entry point

[Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

The [low level API](#low-level-api) is also available for more advanced use cases.

@docs element
@docs document
@docs application
@docs Procedure
@docs Program
@docs Document
@docs Model


# Msg

@docs Msg
@docs issue
@docs publish


# Primitive Procedures

@docs none
@docs batch
@docs modify
@docs push
@docs await
@docs async
@docs sync
@docs race
@docs quit
@docs jump
@docs doUntil
@docs protected
@docs withResource
@docs when
@docs unless
@docs withMaybe


# Observer

@docs Observer
@docs global
@docs dig
@docs setVariant
@docs prepend
@docs append
@docs insertBefore
@docs insertAfter


# Low level API

@docs mapCmd
@docs memoryState
@docs update
@docs init

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Internal.ObserverId as ObserverId exposing (ObserverId)
import Platform
import Url exposing (Url)



-- Entry Point


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model (Cmd (Msg event)) memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document event =
    Browser.Document event


{-| Procedure version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element).
-}
element :
    { init : memory
    , procedures : flags -> List (Procedure (Cmd (Msg event)) memory event)
    , view : memory -> Html (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                init option.init (option.procedures flags)
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
        }


{-| Procedure version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document).
-}
document :
    { init : memory
    , procedures : flags -> List (Procedure (Cmd (Msg event)) memory event)
    , view : memory -> Document (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                init option.init (option.procedures flags)
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
        }


{-| Procedure version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application).
-}
application :
    { init : memory
    , procedures : flags -> Url -> Key -> List (Procedure (Cmd (Msg event)) memory event)
    , view : memory -> Document (Msg event)
    , subscriptions : memory -> Sub (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                init option.init (option.procedures flags url key)
                    |> concatPair
        , view =
            \model -> option.view (memoryState model)
        , update =
            \msg model ->
                update msg model |> concatPair
        , subscriptions =
            \model ->
                option.subscriptions (memoryState model)
        , onUrlRequest = option.onUrlRequest >> issue ObserverId.init
        , onUrlChange = option.onUrlChange >> issue ObserverId.init
        }


concatPair : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
concatPair ( model, cmds ) =
    ( model, Cmd.batch cmds )



-- Core


{-| -}
type Model cmd memory event
    = Thread
        -- New thread state after the evaluation.
        { newState : ThreadState memory

        -- Side effects caused by the evaluation.
        , cmds : List cmd

        -- New thread to evaluate next time.
        , next : Msg event -> ThreadState memory -> Thread cmd memory event
        }


type alias Thread cmd memory event =
    Model cmd memory event


{-| -}
memoryState : Model cmd memory event -> memory
memoryState (Thread { newState }) =
    newState.memory


{-| -}
update : Msg event -> Model cmd memory event -> ( Model cmd memory event, List cmd )
update msg (Thread t) =
    let
        (Thread t2) =
            t.next msg t.newState
    in
    ( Thread t2, t2.cmds )


{-| -}
init :
    memory
    -> List (Procedure cmd memory event)
    -> ( Model cmd memory event, List cmd )
init initialMemory procs =
    let
        (Procedure items) =
            batch procs

        (Thread t) =
            toThread <|
                fromProcedure
                    { memory = initialMemory
                    , nextObserverId = ObserverId.inc ObserverId.init
                    }
                    items
    in
    ( Thread t, t.cmds )


{-| -}
type Msg event
    = Msg ObserverId event


{-| Issue an event to the observer specified by the `ObserverId`.
-}
issue : ObserverId -> event -> Msg event
issue =
    Msg


{-| Issue an event to the global observer.
-}
publish : event -> Msg event
publish =
    Msg ObserverId.init


{-| State to evaluate a thread.
-}
type alias ThreadState memory =
    { memory : memory
    , nextObserverId : ObserverId
    }


{-| Intermediate type, which helps to handle operations that affects ancestor threads.
-}
type FromProcedure cmd memory event
    = FromProcedure
        { newState : ThreadState memory
        , cmds : List cmd
        , next :
            Maybe
                { procedure : Msg event -> ThreadState memory -> FromProcedure cmd memory event
                , onKilled : memory -> ( memory, List cmd )
                }
        }


toThread : FromProcedure cmd memory event -> Thread cmd memory event
toThread (FromProcedure fp) =
    case fp.next of
        Nothing ->
            Thread
                { newState = fp.newState
                , cmds = fp.cmds
                , next = endOfThread
                }

        Just next ->
            Thread
                { newState = fp.newState
                , cmds = fp.cmds
                , next = \msg s -> toThread (next.procedure msg s)
                }


endOfThread : Msg event -> ThreadState memory -> Thread cmd memory event
endOfThread _ state =
    Thread
        { newState = state
        , cmds = []
        , next = endOfThread
        }


type ProcedureItem cmd memory event
    = Do (memory -> ( memory, List cmd ))
    | Await (Msg event -> memory -> Maybe (List (ProcedureItem cmd memory event)))
      -- Run concurrently in new thread, killed when the parent thread is killed.
      -- The parent thread keep alive if the new thread alives.
    | Async (List (ProcedureItem cmd memory event))
    | Observe
        (ObserverId
         -> memory
         ->
            ( memory -- new memory state after resource assignment.
            , List (ProcedureItem cmd memory event) -- Procedures for assigned resource.
            , memory
              -> ( memory, List cmd ) -- Evaluated when the second element of this tuple ends.
            )
        )
    | Sync (List (List (ProcedureItem cmd memory event)))
    | Race (List (List (ProcedureItem cmd memory event)))
      -- Ignore subsequent `Procedure`s and run given `Procedure`s in current thread.
    | Jump (memory -> List (ProcedureItem cmd memory event))
    | Quit


fromProcedure : ThreadState memory -> List (ProcedureItem cmd memory event) -> FromProcedure cmd memory event
fromProcedure state procs =
    case procs of
        [] ->
            FromProcedure
                { newState = state
                , cmds = []
                , next = Nothing
                }

        (Do f) :: ps ->
            let
                ( memory1, cmds1 ) =
                    f state.memory

                state1 =
                    { state | memory = memory1 }

                (FromProcedure fp1) =
                    fromProcedure state1 ps
            in
            FromProcedure { fp1 | cmds = cmds1 ++ fp1.cmds }

        (Await f) :: ps2 ->
            FromProcedure
                { newState = state
                , cmds = []
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                case f msg s.memory of
                                    Nothing ->
                                        fromProcedure s procs

                                    Just ps1 ->
                                        fromProcedure s (ps1 ++ ps2)
                        , onKilled = \m -> ( m, [] )
                        }
                }

        (Async ps1) :: ps2 ->
            fromProcedure state ps1
                |> andAsync (\s -> fromProcedure s ps2)

        (Observe f) :: ps2 ->
            let
                ( memory1, ps1, finally ) =
                    f state.nextObserverId state.memory

                state1 =
                    { memory = memory1
                    , nextObserverId = ObserverId.inc state.nextObserverId
                    }
            in
            fromProcedure state1 ps1
                |> applyFinally finally
                |> andThen (\s -> fromProcedure s ps2)

        (Sync ps) :: ps2 ->
            fromProcDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Race ps) :: ps2 ->
            fromProcRaceDeps state ps
                |> andThen (\s -> fromProcedure s ps2)

        (Jump f) :: _ ->
            fromProcedure state (f state.memory)

        Quit :: _ ->
            endOfProcedure state


endOfProcedure : ThreadState memory -> FromProcedure cmd memory event
endOfProcedure s =
    FromProcedure
        { newState = s
        , cmds = []
        , next = Nothing
        }


applyFinally : (memory -> ( memory, List cmd )) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
applyFinally f (FromProcedure fp) =
    case fp.next of
        Nothing ->
            let
                state1 =
                    fp.newState

                ( memory1, cmds1 ) =
                    f state1.memory

                state2 =
                    { state1 | memory = memory1 }
            in
            FromProcedure
                { newState = state2
                , cmds = fp.cmds ++ cmds1
                , next = Nothing
                }

        Just next ->
            FromProcedure
                { fp
                    | next =
                        Just
                            { procedure =
                                \msg s ->
                                    next.procedure msg s
                                        |> applyFinally f
                            , onKilled = mergeUpdates f next.onKilled
                            }
                }


mergeUpdates : (memory -> ( memory, List cmd )) -> (memory -> ( memory, List cmd )) -> memory -> ( memory, List cmd )
mergeUpdates f g memory =
    let
        ( memory1, cmds1 ) =
            f memory

        ( memory2, cmds2 ) =
            g memory1
    in
    ( memory2, cmds1 ++ cmds2 )


{-| Run a function after the given `FromProcedure` ends.
-}
andThen : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andThen f (FromProcedure fp) =
    case fp.next of
        Nothing ->
            let
                (FromProcedure fp2) =
                    f fp.newState
            in
            FromProcedure
                { fp2 | cmds = fp.cmds ++ fp2.cmds }

        Just next ->
            FromProcedure
                { fp
                    | next =
                        Just
                            { procedure =
                                \msg s ->
                                    next.procedure msg s
                                        |> andThen f
                            , onKilled = next.onKilled
                            }
                }


andAsync : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andAsync f (FromProcedure fp1) =
    let
        (FromProcedure fp2) =
            f fp1.newState
    in
    case ( fp1.next, fp2.next ) of
        ( Nothing, _ ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = fp2.next
                }

        ( _, Nothing ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = fp1.next
                }

        ( Just next1, Just next2 ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                next1.procedure msg s
                                    |> andAsync (next2.procedure msg)
                        , onKilled =
                            mergeUpdates next1.onKilled next2.onKilled
                        }
                }


{-| Merge dependent procedures for `Sync` into one procedure.
-}
fromProcDeps : ThreadState memory -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
fromProcDeps state ps =
    List.foldl
        (\p acc ->
            acc
                |> andNextDep (\s -> fromProcedure s p)
        )
        (FromProcedure
            { newState = state
            , cmds = []
            , next = Nothing
            }
        )
        ps


andNextDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextDep =
    andAsync


{-| Merge dependent procedures for `Race` into one procedure.
If given empty list, it skips to next `Procedure`.
-}
fromProcRaceDeps : ThreadState memory -> List (List (ProcedureItem cmd memory event)) -> FromProcedure cmd memory event
fromProcRaceDeps state ps =
    case ps of
        [] ->
            FromProcedure
                { newState = state
                , cmds = []
                , next = Nothing
                }

        q :: qs ->
            List.foldl
                (\p acc ->
                    acc
                        |> andNextRaceDep (\s -> fromProcedure s p)
                )
                (fromProcedure state q)
                qs


andNextRaceDep : (ThreadState memory -> FromProcedure cmd memory event) -> FromProcedure cmd memory event -> FromProcedure cmd memory event
andNextRaceDep f (FromProcedure fp1) =
    let
        (FromProcedure fp2) =
            f fp1.newState
    in
    case ( fp1.next, fp2.next ) of
        ( Nothing, Nothing ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next = Nothing
                }

        ( Nothing, Just next2 ) ->
            let
                newState2 =
                    fp2.newState

                ( memory3, cmds3 ) =
                    next2.onKilled newState2.memory

                newState3 =
                    { newState2 | memory = memory3 }
            in
            FromProcedure
                { newState = newState3
                , cmds = fp1.cmds ++ fp2.cmds ++ cmds3
                , next = Nothing
                }

        ( Just next1, Nothing ) ->
            let
                newState2 =
                    fp2.newState

                ( memory3, cmds3 ) =
                    next1.onKilled newState2.memory

                newState3 =
                    { newState2 | memory = memory3 }
            in
            FromProcedure
                { newState = newState3
                , cmds = fp1.cmds ++ fp2.cmds ++ cmds3
                , next = Nothing
                }

        ( Just next1, Just next2 ) ->
            FromProcedure
                { newState = fp2.newState
                , cmds = fp1.cmds ++ fp2.cmds
                , next =
                    Just
                        { procedure =
                            \msg s ->
                                next1.procedure msg s
                                    |> andNextRaceDep (next2.procedure msg)
                        , onKilled =
                            mergeUpdates next1.onKilled next2.onKilled
                        }
                }


{-| -}
type Procedure cmd memory event
    = Procedure (List (ProcedureItem cmd memory event))


{-| -}
none : Procedure cmd memory event
none =
    Procedure []


{-| -}
batch : List (Procedure cmd memory event) -> Procedure cmd memory event
batch procs =
    List.concatMap (\(Procedure ps) -> ps) procs
        |> Procedure


{-| Use to lift memory type.
-}
type alias Lifter a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


{-| Use to convert local event types.
-}
type alias Wrapper a b =
    { unwrap : a -> Maybe b
    , wrap : b -> a
    }


{-| -}
mapCmd : (a -> b) -> Procedure a memory event -> Procedure b memory event
mapCmd set (Procedure ps) =
    List.map (mapCmd_ set) ps
        |> Procedure


{-| -}
mapCmd_ : (a -> b) -> ProcedureItem a memory event -> ProcedureItem b memory event
mapCmd_ set pb =
    case pb of
        Do f ->
            Do <|
                \memory ->
                    let
                        ( memory2, cmds ) =
                            f memory
                    in
                    ( memory2
                    , List.map set cmds
                    )

        Await f ->
            Await <|
                \msg memory ->
                    f msg memory
                        |> Maybe.map (List.map (mapCmd_ set))

        Async ps ->
            Async <| List.map (mapCmd_ set) ps

        Observe f ->
            Observe <|
                \rid memory ->
                    let
                        ( memory1, ps, finally_ ) =
                            f rid memory

                        finally x =
                            let
                                ( x1, cmds ) =
                                    finally_ x
                            in
                            ( x1, List.map set cmds )
                    in
                    ( memory1, List.map (mapCmd_ set) ps, finally )

        Sync ps ->
            Sync <|
                List.map (\p -> List.map (mapCmd_ set) p) ps

        Race ps ->
            Race <|
                List.map (\p -> List.map (mapCmd_ set) p) ps

        Jump f ->
            Jump <| \memory -> List.map (mapCmd_ set) (f memory)

        Quit ->
            Quit


{-| Modifies the shared memory state.
If the given `Observer` has expired, it do nothing.
-}
modify : Observer memory a -> (a -> a) -> Procedure cmd memory event
modify (Observer { lifter }) f =
    Procedure
        [ Do <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        ( memory, [] )

                    Just a ->
                        ( lifter.set (f a) memory, [] )
        ]


{-| Push new `Cmd`.
The second argument takes current shared memory state.

If the given `Observer` has expire, it do nothing.

-}
push : Observer memory a -> (ObserverId -> a -> cmd) -> Procedure cmd memory event
push (Observer { id, lifter }) f =
    Procedure
        [ Do <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        ( memory, [] )

                    Just a ->
                        ( memory, [ f id a ] )
        ]


{-| Await events with the given `Observer`.

If the given `Observer` has expired, it awaits again.
If the second argument returns empty list, it awaits again.

-}
await : Observer memory a -> (event -> a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
await (Observer { id, lifter }) f =
    Procedure
        [ Await <|
            \(Msg rid event) memory ->
                case lifter.get memory of
                    Just a ->
                        if rid == id then
                            case f event a of
                                [] ->
                                    Nothing

                                ps ->
                                    let
                                        (Procedure items) =
                                            batch ps
                                    in
                                    Just items

                        else
                            Nothing

                    Nothing ->
                        Nothing
        ]


{-| Run in asynchronous thread, which ends when the original thread ends.
-}
async : List (Procedure cmd memory event) -> Procedure cmd memory event
async ps =
    let
        (Procedure items) =
            batch ps
    in
    Procedure
        [ Async items
        ]


{-| -}
protected : Observer memory a -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
protected (Observer observer) f =
    observe <|
        \oid memory ->
            ( memory
            , f <|
                Observer
                    { id = oid
                    , lifter = observer.lifter
                    }
            , \m -> ( m, [] )
            )


{-| Aquire a resource, do some work with it, and then release the resource.
-}
withResource :
    Observer memory a
    ->
        { aquire : ObserverId -> a -> ( a, r )
        , release : r -> a -> ( a, List cmd )
        }
    -> (r -> List (Procedure cmd memory event))
    -> Procedure cmd memory event
withResource (Observer { lifter }) { aquire, release } f =
    observe <|
        \rid memory ->
            case lifter.get memory of
                Nothing ->
                    ( memory, [], \m -> ( m, [] ) )

                Just a ->
                    let
                        ( a1, r ) =
                            aquire rid a
                    in
                    ( lifter.set a1 memory
                    , f r
                    , \m ->
                        case lifter.get m of
                            Nothing ->
                                ( m, [] )

                            Just a2 ->
                                let
                                    ( a3, cmds ) =
                                        release r a2
                                in
                                ( lifter.set a3 m, cmds )
                    )


{-| Blocks thread till all the given threads are complete.
-}
sync : List (Procedure cmd memory event) -> Procedure cmd memory event
sync ps =
    Procedure
        [ Sync <|
            List.map (\(Procedure items) -> items) ps
        ]


{-| Blocks thread till one the given threads is complete.
It cancels any other given threads in progress.
-}
race : List (Procedure cmd memory event) -> Procedure cmd memory event
race ps =
    Procedure
        [ Race <|
            List.map (\(Procedure items) -> items) ps
        ]


{-| -}
quit : Procedure cmd memory event
quit =
    Procedure [ Quit ]


{-| -}
jump : Observer memory a -> (a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
jump (Observer { lifter }) f =
    Procedure
        [ Jump <|
            \memory ->
                case lifter.get memory of
                    Nothing ->
                        []

                    Just a ->
                        let
                            (Procedure items) =
                                batch (f a)
                        in
                        items
        ]


{-| -}
doUntil : Observer memory a -> List (Procedure cmd memory event) -> (event -> a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
doUntil o procs handler =
    sync
        [ await o handler
        , race
            [ batch procs
            , await o <|
                \event a ->
                    case handler event a of
                        [] ->
                            []

                        _ ->
                            [ none ]
            ]
        ]


observe :
    (ObserverId
     -> memory
     ->
        ( memory
        , List (Procedure cmd memory event)
        , memory -> ( memory, List cmd )
        )
    )
    -> Procedure cmd memory event
observe f =
    Procedure
        [ Observe <|
            \rid memory ->
                let
                    ( memory2, ps, finally ) =
                        f rid memory

                    (Procedure items) =
                        batch ps
                in
                ( memory2, items, finally )
        ]


{-| -}
when : Bool -> List (Procedure cmd memory event) -> Procedure cmd memory event
when p ls =
    if p then
        batch ls

    else
        none


{-| -}
unless : Bool -> List (Procedure cmd memory event) -> Procedure cmd memory event
unless p =
    when (not p)


{-| -}
withMaybe : Maybe a -> (a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
withMaybe ma f =
    case ma of
        Nothing ->
            none

        Just a ->
            f a
                |> batch



-- Observer


{-| -}
type Observer memory part
    = Observer
        { id : ObserverId
        , lifter : Lifter memory part
        }


{-| -}
global : Observer memory memory
global =
    Observer
        { id = ObserverId.init
        , lifter =
            { get = Just
            , set = \x _ -> x
            }
        }


{-| -}
dig :
    { get : b -> a
    , set : a -> b -> b
    }
    -> Observer memory b
    -> Observer memory a
dig { get, set } (Observer observer) =
    Observer
        { id = observer.id
        , lifter =
            observer.lifter
                |> andCompose
                    { get = get >> Just
                    , set = set
                    }
        }


andCompose : Lifter b c -> Lifter a b -> Lifter a c
andCompose l2 l1 =
    { get =
        \a ->
            l1.get a
                |> Maybe.andThen l2.get
    , set =
        \c a ->
            case l1.get a of
                Nothing ->
                    a

                Just b ->
                    l2.set c b
                        |> (\newb -> l1.set newb a)
    }


{-| -}
setVariant :
    Observer memory a
    ->
        { wrap : ( ObserverId, b ) -> a
        , unwrap : a -> Maybe ( ObserverId, b )
        }
    -> b
    -> (Observer memory b -> List (Procedure cmd memory event))
    -> Procedure cmd memory event
setVariant (Observer parent) wrapper b f =
    withResource
        (Observer
            { id = parent.id
            , lifter =
                { get = Just
                , set = \x _ -> x
                }
            }
        )
        { aquire =
            \rid memory ->
                let
                    r =
                        Observer
                            { id = rid
                            , lifter =
                                parent.lifter
                                    |> andCompose (wrapperToVariantLifter wrapper)
                                    |> andCompose
                                        { get = \( _, x ) -> Just x
                                        , set = \x ( id, _ ) -> ( id, x )
                                        }
                            }
                in
                ( parent.lifter.set (wrapper.wrap ( rid, b )) memory, r )
        , release = \_ m -> ( m, [] )
        }
        f


{-| -}
prepend : Observer memory (List ( ObserverId, a )) -> a -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
prepend observer a =
    listObserver observer (\rid xs -> ( rid, a ) :: xs)


{-| -}
append : Observer memory (List ( ObserverId, a )) -> a -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
append observer a =
    listObserver observer (\rid xs -> xs ++ [ ( rid, a ) ])


{-| If the `Observer` for the base element has expired, the call back function is called, but is passed an expired `Observer`.
-}
insertBefore : Observer memory (List ( ObserverId, a )) -> Observer memory a -> a -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
insertBefore observer (Observer base) a =
    listObserver observer <|
        \newRid xs ->
            List.foldr
                (\( rid, v ) acc ->
                    if rid == base.id then
                        ( newRid, a ) :: ( rid, v ) :: acc

                    else
                        ( rid, v ) :: acc
                )
                []
                xs


{-| If the `Observer` for the base element has expired, the call back function is called, but is passed an expired `Observer`.
-}
insertAfter : Observer memory (List ( ObserverId, a )) -> Observer memory a -> a -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
insertAfter observer (Observer base) a =
    listObserver observer <|
        \newRid xs ->
            List.foldr
                (\( rid, v ) acc ->
                    if rid == base.id then
                        ( rid, v ) :: ( newRid, a ) :: acc

                    else
                        ( rid, v ) :: acc
                )
                []
                xs


listObserver : Observer memory (List ( ObserverId, a )) -> (ObserverId -> List ( ObserverId, a ) -> List ( ObserverId, a )) -> (Observer memory a -> List (Procedure cmd memory event)) -> Procedure cmd memory event
listObserver observer modifier f =
    withResource observer
        { aquire = \rid rls -> ( modifier rid rls, rid )
        , release = \rid rls -> ( remove rid rls, [] )
        }
        (\rid ->
            f
                (Observer
                    { id = rid
                    , lifter =
                        let
                            (Observer { lifter }) =
                                observer
                        in
                        { get =
                            \memory ->
                                lifter.get memory
                                    |> Maybe.andThen (getListItem rid)
                        , set =
                            \v memory ->
                                case lifter.get memory of
                                    Nothing ->
                                        memory

                                    Just rls ->
                                        lifter.set (setListItem rid v rls) memory
                        }
                    }
                )
        )


getListItem : ObserverId -> List ( ObserverId, a ) -> Maybe a
getListItem target ls =
    List.filterMap
        (\( rid, a ) ->
            if rid == target then
                Just a

            else
                Nothing
        )
        ls
        |> List.head


setListItem : ObserverId -> a -> List ( ObserverId, a ) -> List ( ObserverId, a )
setListItem target a ls =
    List.map
        (\( rid, v ) ->
            if rid == target then
                ( rid, a )

            else
                ( rid, v )
        )
        ls


remove : ObserverId -> List ( ObserverId, a ) -> List ( ObserverId, a )
remove target ls =
    List.filter (\( rid, _ ) -> rid /= target) ls


wrapperToVariantLifter : Wrapper a b -> Lifter a b
wrapperToVariantLifter wrapper =
    { get = wrapper.unwrap
    , set =
        \a memory ->
            case wrapper.unwrap memory of
                Nothing ->
                    memory

                Just _ ->
                    wrapper.wrap a
    }
