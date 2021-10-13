module Thread.Browser exposing
    ( element
    , document
    , application
    , Program
    , Document
    , Model
    , Msg
    )

{-| [Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

If your needs cannot be met by this module, use the low level functions of [`Thread.Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure).

@docs element
@docs document
@docs application
@docs Program
@docs Document
@docs Model
@docs Msg

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Platform
import Thread.Procedure as Procedure exposing (Procedure)
import Url exposing (Url)



-- Browser.* alternatives


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags shared global local =
    Platform.Program flags (Model shared global local) (Msg global local)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document global =
    Browser.Document global


{-| Threads version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element)
-}
element :
    { init : shared
    , procedure : flags -> Procedure shared global local
    , view : shared -> Html global
    , subscriptions : shared -> Sub global
    }
    -> Program flags shared global local
element option =
    Browser.element
        { init = \flags -> Procedure.init option.init (option.procedure flags)
        , view = Procedure.elementView option.view
        , update = Procedure.update
        , subscriptions = Procedure.subscriptions option.subscriptions
        }


{-| Threads version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document)
-}
document :
    { init : shared
    , procedure : flags -> Procedure shared global local
    , view : shared -> Document global
    , subscriptions : shared -> Sub global
    }
    -> Program flags shared global local
document option =
    Browser.document
        { init = \flags -> Procedure.init option.init (option.procedure flags)
        , view = Procedure.documentView option.view
        , update = Procedure.update
        , subscriptions = Procedure.subscriptions option.subscriptions
        }


{-| Threads version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
-}
application :
    { init : shared
    , procedure : flags -> Url -> Key -> Procedure shared global local
    , view : shared -> Document global
    , subscriptions : shared -> Sub global
    , onUrlRequest : Browser.UrlRequest -> global
    , onUrlChange : Url -> global
    }
    -> Program flags shared global local
application option =
    Browser.application
        { init = \flags url key -> Procedure.init option.init (option.procedure flags url key)
        , view = Procedure.documentView option.view
        , update = Procedure.update
        , subscriptions = Procedure.subscriptions option.subscriptions
        , onUrlRequest = Procedure.onUrlRequest option.onUrlRequest
        , onUrlChange = Procedure.onUrlChange option.onUrlChange
        }


{-| Reexport `Thread.Procedure.Model` for convenience.
-}
type alias Model shared global local =
    Procedure.Model shared global local


{-| Reexport `Thread.Procedure.Msg` for convenience.
-}
type alias Msg global local =
    Procedure.Msg global local
