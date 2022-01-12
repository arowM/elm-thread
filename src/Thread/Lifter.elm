module Thread.Lifter exposing
    ( Lifter
    , compose
    , cond
    , modify
    , push
    , await
    , async
    , block
    , sync
    , race
    , jump
    , doUntil
    , addFinalizer
    , modifyAndThen
    , when
    , unless
    , withMemory
    )

{-| Helper module for converting [`Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#Procedure).


# Core

@docs Lifter
@docs compose
@docs cond


# Thread.Procedure alternatives

@docs modify
@docs push
@docs await
@docs async
@docs block
@docs sync
@docs race
@docs jump
@docs doUntil
@docs addFinalizer
@docs modifyAndThen
@docs when
@docs unless
@docs withMemory

-}

import Thread.Procedure as Procedure exposing (Block, Procedure)


{-| Reexport `Thread.Procedure.Lifter` for convenience.
-}
type alias Lifter a b =
    { get : a -> Maybe b
    , set : b -> a -> a
    }


{-| -}
compose : Lifter a b -> Lifter b c -> Lifter a c
compose l1 l2 =
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
cond : Lifter a b -> (b -> Bool) -> a -> Bool
cond lifter f a =
    case lifter.get a of
        Just b ->
            f b

        Nothing ->
            False


{-| -}
modify : Lifter a b -> (b -> b) -> Procedure a event
modify l f =
    Procedure.modify f
        |> Procedure.lift l


{-| -}
push : Lifter a b -> (b -> Cmd event) -> Procedure a event
push l f =
    Procedure.push f
        |> Procedure.lift l


{-| Lifter version of `await`.

It awaits again if the `get` of the given `Lifter` returns `Nothing` for current memory `a`.

-}
await : Lifter a b -> (event -> b -> List (Procedure a event)) -> Procedure a event
await l f =
    Procedure.await <|
        \event a ->
            case l.get a of
                Nothing ->
                    []

                Just b ->
                    f event b


{-| -}
async : Lifter a b -> (Lifter a b -> Block a event) -> Procedure a event
async l f =
    Procedure.async (f l)


{-| -}
block : Lifter a b -> (Lifter a b -> Block a event) -> Procedure a event
block l f =
    Procedure.block (f l)


{-| -}
sync : Lifter a b -> List (Lifter a b -> Block a event) -> Procedure a event
sync l fs =
    List.map (\f -> f l) fs
        |> Procedure.sync


{-| -}
race : Lifter a b -> List (Lifter a b -> Block a event) -> Procedure a event
race l fs =
    List.map (\f -> f l) fs
        |> Procedure.race


{-| -}
jump : Lifter a b -> (Lifter a b -> Block a event) -> Procedure a event
jump l f =
    Procedure.jump (f l)


{-| -}
doUntil : Lifter a b -> (Lifter a b -> Block a event) -> (event -> a -> List (Procedure a event)) -> Procedure a event
doUntil l f =
    Procedure.doUntil (f l)


{-| Lifter version of `addFinalizer`.

It returns `none` if the `get` of the given `Lifter` returns `Nothing` for current memory `a`.

-}
addFinalizer : Lifter a b -> (Lifter a b -> Block a event) -> Procedure a event
addFinalizer l f =
    Procedure.addFinalizer (f l)


{-| -}
modifyAndThen : Lifter a b -> (b -> ( b, x )) -> (x -> Lifter a b -> Block a event) -> Procedure a event
modifyAndThen l f g =
    Procedure.modifyAndThen
        (\a ->
            case l.get a of
                Nothing ->
                    ( a, Nothing )

                Just b ->
                    let
                        ( newB, x ) =
                            f b
                    in
                    ( l.set newB a, Just x )
        )
        (\mx ->
            case mx of
                Nothing ->
                    \_ -> []

                Just x ->
                    g x l
        )


{-| Lifter version of `when`.

It returns `none` if the `get` of the given `Lifter` returns `Nothing` for current memory `a`.

-}
when : Lifter a b -> (b -> Bool) -> List (Procedure a event) -> Procedure a event
when l f =
    Procedure.when
        (\a ->
            case l.get a of
                Nothing ->
                    False

                Just b ->
                    f b
        )


{-| Lifter version of `unless`.

It returns `none` if the `get` of the given `Lifter` returns `Nothing` for current memory `a`.

-}
unless : Lifter a b -> (b -> Bool) -> List (Procedure a event) -> Procedure a event
unless l f =
    Procedure.unless
        (\a ->
            case l.get a of
                Nothing ->
                    True

                Just b ->
                    f b
        )


{-| Lifter version of `withMemory`.

It returns `none` if the `get` of the given `Lifter` returns `Nothing` for current memory `a`.

-}
withMemory : Lifter a b -> (b -> Block a event) -> Procedure a event
withMemory l f =
    Procedure.withMemory <|
        \a ->
            case l.get a of
                Nothing ->
                    \_ -> []

                Just b ->
                    f b
