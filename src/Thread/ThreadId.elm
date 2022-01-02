module Thread.ThreadId exposing
    ( ThreadId
    , toString
    , decoder
    , toValue
    , init
    )

{-|


# Core

@docs ThreadId
@docs toString


# JSON

@docs decoder
@docs toValue


# Lower level functions

@docs init

-}

import Internal.ThreadId as Internal
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| ID to determine threads.
-}
type alias ThreadId =
    Internal.ThreadId


{-| Convert into `String`.
Different `ThreadId`s will be converted to different strings, and the same `ThreadId`s will always be converted to the same string.
-}
toString : ThreadId -> String
toString =
    Internal.toString


{-| JSON decoder.
-}
decoder : Decoder ThreadId
decoder =
    Internal.decoder


{-| Convert into JSON `Value`.
-}
toValue : ThreadId -> Value
toValue =
    Internal.toValue


{-| `ThreadId` for entry point thread.
-}
init : ThreadId
init =
    Internal.init
