module Thread.Wrapper exposing
    ( Wrapper
    , compose
    )

{-| Helper module for converting [`Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure#Procedure).

@docs Wrapper
@docs compose

-}


{-| Use to convert local event types.
-}
type alias Wrapper a b =
    { unwrap : a -> Maybe b
    , wrap : b -> a
    }


{-| -}
compose : Wrapper a b -> Wrapper b c -> Wrapper a c
compose w1 w2 =
    { unwrap = w1.unwrap >> Maybe.andThen w2.unwrap
    , wrap = w2.wrap >> w1.wrap
    }
