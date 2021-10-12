module Internal.ThreadId exposing
    ( ThreadId
    , init
    , inc
    )

{-|

@docs ThreadId
@docs init
@docs inc

-}


{-| ID to determine to which thread a local message is addressed.
-}
type ThreadId
    = ThreadId Int


{-| Initial value for `ThreadId`.
-}
init : ThreadId
init =
    ThreadId 0


{-| Increment `ThreadId`.
-}
inc : ThreadId -> ThreadId
inc (ThreadId n) =
    ThreadId (n + 1)
