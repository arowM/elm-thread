module Thread.Browser exposing
    ( element
    , document
    , application
    , Program
    , Document
    , globalHtml
    , globalDocument
    , globalSubscriptions
    )

{-| [Browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser) alternatives.

If your needs cannot be met by this module, use the low level functions of [`Thread.Procedure`](https://package.elm-lang.org/packages/arowM/elm-thread/latest/Thread-Procedure).

@docs element
@docs document
@docs application
@docs Program
@docs Document
@docs globalHtml
@docs globalDocument
@docs globalSubscriptions

-}

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Platform
import Thread.Procedure as Procedure exposing (Block, Model, Msg, ThreadId)
import Url exposing (Url)



-- Browser.* alternatives


{-| An alias for [Platform.Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program).
-}
type alias Program flags memory event =
    Platform.Program flags (Model memory event) (Msg event)


{-| Reexport [Browser.Document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#Document) for convenience.
-}
type alias Document event =
    Browser.Document event


{-| Threads version of [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element)
-}
element :
    { init : memory
    , procedures : flags -> Block memory event
    , view : ThreadId -> memory -> Html (Msg event)
    , subscriptions : ThreadId -> memory -> Sub (Msg event)
    }
    -> Program flags memory event
element option =
    Browser.element
        { init =
            \flags ->
                Procedure.init option.init
                    (option.procedures flags)
        , view =
            \model ->
                Procedure.extractMemory model
                    |> option.view Procedure.initThreadId
        , update = Procedure.update
        , subscriptions =
            \model ->
                Procedure.extractMemory model
                    |> option.subscriptions Procedure.initThreadId
        }


{-| Threads version of [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document)
-}
document :
    { init : memory
    , procedures : flags -> Block memory event
    , view : ThreadId -> memory -> Document (Msg event)
    , subscriptions : ThreadId -> memory -> Sub (Msg event)
    }
    -> Program flags memory event
document option =
    Browser.document
        { init =
            \flags ->
                Procedure.init option.init
                    (option.procedures flags)
        , view =
            \model ->
                Procedure.extractMemory model
                    |> option.view Procedure.initThreadId
        , update = Procedure.update
        , subscriptions =
            \model ->
                Procedure.extractMemory model
                    |> option.subscriptions Procedure.initThreadId
        }


{-| Threads version of [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
-}
application :
    { init : memory
    , procedures : flags -> Url -> Key -> Block memory event
    , view : ThreadId -> memory -> Document (Msg event)
    , subscriptions : ThreadId -> memory -> Sub (Msg event)
    , onUrlRequest : Browser.UrlRequest -> event
    , onUrlChange : Url -> event
    }
    -> Program flags memory event
application option =
    Browser.application
        { init =
            \flags url key ->
                Procedure.init option.init
                    (option.procedures flags url key)
        , view =
            \model ->
                Procedure.extractMemory model
                    |> option.view Procedure.initThreadId
        , update = Procedure.update
        , subscriptions =
            \model ->
                Procedure.extractMemory model
                    |> option.subscriptions Procedure.initThreadId
        , onUrlRequest = option.onUrlRequest >> Procedure.setTarget Procedure.initThreadId
        , onUrlChange = option.onUrlChange >> Procedure.setTarget Procedure.initThreadId
        }


{-| Construct a `view` field for `element` from `Html event`. All the events fired in the resulting `Html` is delivered to all threads.
-}
globalHtml : (memory -> Html event) -> ThreadId -> memory -> Html (Msg event)
globalHtml f tid memory =
    f memory
        |> Html.map (Procedure.setTarget tid)


{-| Construct a `view` field for `document` and `application` from `Document event`. All the events fired in the resulting `Document` is delivered to all threads.
-}
globalDocument : (memory -> Document event) -> ThreadId -> memory -> Document (Msg event)
globalDocument f tid memory =
    let
        { title, body } =
            f memory
    in
    { title = title
    , body =
        body
            |> List.map (Html.map (Procedure.setTarget tid))
    }


{-| Construct a `subscriptions` field from `Sub event`. All the events fired in the resulting `Sub` is delivered to all threads.
-}
globalSubscriptions : (memory -> Sub event) -> ThreadId -> memory -> Sub (Msg event)
globalSubscriptions f tid memory =
    f memory
        |> Sub.map (Procedure.setTarget tid)
