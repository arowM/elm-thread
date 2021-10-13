module Thread.Lifter exposing
    ( Lifter
    , compose
    )

{-| Helper module for converting [`Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#Procedure).

@docs Lifter
@docs compose

-}


{-| Use to convert shared memory types.
-}
type alias Lifter a b =
    { get : a -> b
    , set : b -> a -> a
    }


{-| -}
compose : Lifter a b -> Lifter b c -> Lifter a c
compose l1 l2 =
    { get = l1.get >> l2.get
    , set =
        \c a ->
            l1.get a
                |> l2.set c
                |> (\b -> l1.set b a)
    }
