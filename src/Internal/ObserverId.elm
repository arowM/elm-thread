module Internal.ObserverId exposing
    ( ObserverId
    , init
    , toString
    , decoder
    , toValue
    , inc
    )

{-|

@docs ObserverId
@docs init
@docs toString
@docs decoder
@docs toValue
@docs inc

-}

import Internal.SafeInt as SafeInt exposing (SafeInt)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| ID to determine to which thread a local message is addressed.
-}
type ObserverId
    = ObserverId (List SafeInt)


{-| Initial value for `ObserverId`.
-}
init : ObserverId
init =
    ObserverId [ SafeInt.minBound ]


{-| Next `ObserverId`.
-}
inc : ObserverId -> ObserverId
inc (ObserverId ls) =
    case incList ls of
        ( True, new ) ->
            ObserverId <| SafeInt.minBound :: new

        ( False, new ) ->
            ObserverId new


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
Different `ObserverId`s will be converted to different strings, and the same `ObserverId`s will always be converted to the same string.
-}
toString : ObserverId -> String
toString (ObserverId ls) =
    List.map SafeInt.toString ls
        |> String.join "_"
        |> (\str -> "tid_" ++ str)


{-| JSON decoder.
-}
decoder : Decoder ObserverId
decoder =
    JD.list SafeInt.decoder
        |> JD.map ObserverId


{-| Convert into JSON `Value`.
-}
toValue : ObserverId -> Value
toValue (ObserverId ls) =
    JE.list SafeInt.toValue ls
