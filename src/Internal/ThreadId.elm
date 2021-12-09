module Internal.ThreadId exposing
    ( ThreadId
    , init
    , inc
    , toString
    )

{-|

@docs ThreadId
@docs init
@docs inc
@docs toString

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)


{-| ID to determine to which thread a local message is addressed.
-}
type ThreadId
    = ThreadId (List SafeInt)


{-| Initial value for `ThreadId`.
-}
init : ThreadId
init =
    ThreadId [ SafeInt.minBound ]


{-| Increment `ThreadId`.
-}
inc : ThreadId -> ThreadId
inc (ThreadId ls) =
    case incList ls of
        ( True, new ) ->
            ThreadId <| SafeInt.minBound :: new

        ( False, new ) ->
            ThreadId new


incList : List SafeInt -> ( Bool, List SafeInt )
incList =
    List.foldr
        (\a ( carry, ls ) ->
            if carry then
                SafeInt.inc a
                    |> Tuple.mapSecond (\new -> new :: ls)

            else
                ( False, a :: ls )
        )
        ( True, [] )


{-| Convert into `String`.
Different `ThreadId`s will be converted to different strings, and the same `ThreadId`s will always be converted to the same string.
-}
toString : ThreadId -> String
toString (ThreadId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)
