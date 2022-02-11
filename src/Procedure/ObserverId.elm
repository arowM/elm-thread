module Procedure.ObserverId exposing
    ( ObserverId
    , decoder
    , toString
    , toValue
    )

{-|

@docs ObserverId
@docs decoder
@docs toString
@docs toValue

-}

import Internal.ObserverId as Internal
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| -}
type alias ObserverId =
    Internal.ObserverId


{-| Convert into `String`.
Different `ObserverId`s are supposed to be converted to different strings, and the same `ObserverId`s to the same string.
-}
toString : ObserverId -> String
toString =
    Internal.toString


{-| JSON decoder.
-}
decoder : Decoder ObserverId
decoder =
    Internal.decoder


{-| Convert into JSON `Value`.
-}
toValue : ObserverId -> Value
toValue =
    Internal.toValue
