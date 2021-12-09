module Thread.Lifter exposing
    ( Lifter
    , compose
    , cond
    )

{-| Helper module for converting [`Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#Procedure).

@docs Lifter
@docs compose
@docs cond

-}


{-| Use to convert shared memory types.
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
