module Internal.SafeInt exposing
    ( SafeInt
    , minBound
    , maxBound
    , inc
    , toString
    )

{-|

@docs SafeInt
@docs minBound
@docs maxBound
@docs inc
@docs toString

-}


{-| Integers guaranteed not to exceed safe limits.
-}
type SafeInt
    = SafeInt Int


{-| -}
minBound : SafeInt
minBound =
    SafeInt minBound_


minBound_ : Int
minBound_ =
    -2147483648


{-| -}
maxBound : SafeInt
maxBound =
    SafeInt maxBound_


maxBound_ : Int
maxBound_ =
    2147483647


{-| Increment safely. If it overflows, the first element of the tuple will be `True` and the second element will be `minBound`.

    Tuple.first (inc minBound)
    --> False

    Tuple.first (inc <| inc minBound)
    --> False

    inc maxBound
    --> ( True, minBound )

-}
inc : SafeInt -> ( Bool, SafeInt )
inc (SafeInt n) =
    if n >= maxBound_ then
        ( True, minBound )

    else
        ( False, SafeInt (n + 1) )


{-| -}
toString : SafeInt -> String
toString (SafeInt n) =
    String.fromInt n
