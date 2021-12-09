module Thread.LocalMemory exposing
    ( LocalMemory
    , init
    , toList
    , asyncChild
    , blockChild
    , async
    , block
    )

{-| Helper module that enable to manage the local memory state of each thread.
For a sample, see [`sample/src/Advanced.elm`](https://github.com/arowM/elm-thread/tree/main/sample/src) and its [live demo](https://arowm.github.io/elm-thread/advanced.html).


# Core

@docs LocalMemory
@docs init
@docs toList


# Procedures

@docs asyncChild
@docs blockChild
@docs async
@docs block

-}

import Thread.Lifter exposing (Lifter)
import Thread.Procedure as Procedure exposing (Block, Procedure, ThreadId)


{-| Core type to store local memory states.
-}
type LocalMemory a
    = LocalMemory (LocalMemory_ a)


type alias LocalMemory_ a =
    -- reverserd
    { list : List ( ThreadId, a )
    }


{-| Construct an initial instance of `LocalMemory`.
-}
init : LocalMemory a
init =
    LocalMemory
        { list = []
        }


{-| Convert into list. The elements in this list are ordered by the time the thread was created, from newest to oldest.
-}
toList : LocalMemory a -> List ( ThreadId, a )
toList (LocalMemory a) =
    a.list


{-| Get the value associated with `ThreadId`. If the `ThreadId` is not found, return `Nothing`.
-}
get : ThreadId -> LocalMemory a -> Maybe a
get target (LocalMemory local) =
    List.filterMap
        (\( id, a ) ->
            if id == target then
                Just a

            else
                Nothing
        )
        local.list
        |> List.head


{-| Assign a new local memory. Do not check if the `ThreadId` already exists.
-}
assign : ThreadId -> a -> LocalMemory a -> LocalMemory a
assign tid initial (LocalMemory local) =
    LocalMemory
        { local
            | list = ( tid, initial ) :: local.list
        }


{-| Remove a local memory. Returns given state as it is if no such `ThreadId` exists.
-}
remove : ThreadId -> LocalMemory a -> LocalMemory a
remove target (LocalMemory local) =
    LocalMemory
        { local
            | list = List.filter (\( tid, _ ) -> tid /= target) local.list
        }



-- Procedures


{-| Run a child `Block` in a new thread by [`Thread.Procedure.async`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#async), assign a local memory for the thread, and free the local memory when the thread is end.

The second argument is initial value of the local memory.

-}
asyncChild :
    Lifter memory (LocalMemory a)
    -> a
    -> Block a event
    -> Procedure memory event
asyncChild lifter def f =
    async lifter def <| \lift -> lift f


{-| Similar to `asyncChild`, but `async` can also handle parent `Procedure`s in the new thread.
-}
async :
    Lifter memory (LocalMemory a)
    -> a
    -> ((Block a event -> Block memory event) -> Block memory event)
    -> Procedure memory event
async lifter def f =
    Procedure.async <|
        \tid ->
            [ Procedure.modify <|
                \memory ->
                    case lifter.get memory of
                        Just old ->
                            lifter.set (assign tid def old) memory

                        Nothing ->
                            memory
            , Procedure.addFinalizer <|
                \_ ->
                    [ Procedure.modify <|
                        \memory ->
                            case lifter.get memory of
                                Just old ->
                                    lifter.set (remove tid old) memory

                                Nothing ->
                                    memory
                    ]
            , f (Procedure.liftBlock (localLifter tid lifter)) tid
                |> Procedure.batch
            ]


{-| Run a child `Block` by [`Thread.Procedure.block`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#block), assign a local memory for the thread, and free the local memory when the thread is end.

The second argument is initial value of the local memory.

-}
blockChild :
    Lifter memory (LocalMemory a)
    -> a
    -> Block a event
    -> Procedure memory event
blockChild lifter def f =
    block lifter def <| \lift -> lift f


{-| Similar to `blockChild`, but `block` can also handle parent `Procedure`s in the new thread.
-}
block :
    Lifter memory (LocalMemory a)
    -> a
    -> ((Block a event -> Block memory event) -> Block memory event)
    -> Procedure memory event
block lifter def f =
    Procedure.block <|
        \tid ->
            [ Procedure.modify <|
                \memory ->
                    case lifter.get memory of
                        Just old ->
                            lifter.set (assign tid def old) memory

                        Nothing ->
                            memory
            , Procedure.addFinalizer <|
                \_ ->
                    [ Procedure.modify <|
                        \memory ->
                            case lifter.get memory of
                                Just old ->
                                    lifter.set (remove tid old) memory

                                Nothing ->
                                    memory
                    ]
            , f (Procedure.liftBlock (localLifter tid lifter)) tid
                |> Procedure.batch
            ]


localLifter : ThreadId -> Lifter memory (LocalMemory a) -> Lifter memory a
localLifter target lifter =
    { get =
        \memory ->
            lifter.get memory
                |> Maybe.andThen (get target)
    , set =
        \new memory ->
            case lifter.get memory of
                Nothing ->
                    memory

                Just (LocalMemory local) ->
                    lifter.set
                        (LocalMemory
                            { local
                                | list =
                                    List.map
                                        (\( id, old ) ->
                                            ( id
                                            , if id == target then
                                                new

                                              else
                                                old
                                            )
                                        )
                                        local.list
                            }
                        )
                        memory
    }
