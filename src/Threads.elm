module Threads exposing
    ( element
    , document
    , application
    , Program
    , Document
    , Model
    , Msg
    , Procedure
    , none
    , batch
    , modify
    , push
    , await
    , awaitGlobal
    , fork
    , syncAll
    , quit
    , elementView
    , documentView
    , init
    , update
    , subscriptions
    , onUrlRequest
    , onUrlChange
    )

{-|


# Core

@docs element
@docs document
@docs application
@docs Program
@docs Document
@docs Model
@docs Msg


# Procedure

@docs Procedure
@docs none
@docs batch
@docs modify
@docs push
@docs await
@docs awaitGlobal
@docs fork
@docs syncAll
@docs quit


# Lower level functions

@docs elementView
@docs documentView
@docs init
@docs update
@docs subscriptions
@docs onUrlRequest
@docs onUrlChange

-}

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Internal
import Internal.ThreadId exposing (ThreadId)
import Platform
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
        { init = \flags -> init option.init (option.procedure flags)
        , view = elementView option.view
        , update = update
        , subscriptions = subscriptions option.subscriptions
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
        { init = \flags -> init option.init (option.procedure flags)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions option.subscriptions
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
        { init = \flags url key -> init option.init (option.procedure flags url key)
        , view = documentView option.view
        , update = update
        , subscriptions = subscriptions option.subscriptions
        , onUrlRequest = onUrlRequest option.onUrlRequest
        , onUrlChange = onUrlChange option.onUrlChange
        }


{-| -}
type Model shared global local
    = Model (Model_ shared global local)


type alias Model_ shared global local =
    { thread : Internal.Thread (Cmd local) shared global local
    , state : Internal.ThreadState shared
    }


{-| -}
elementView : (shared -> Html global) -> Model shared global local -> Html (Msg global local)
elementView f (Model model) =
    f model.state.shared
        |> Html.map globalEvent


{-| -}
documentView : (shared -> Document global) -> Model shared global local -> Document (Msg global local)
documentView f (Model model) =
    let
        doc =
            f model.state.shared
    in
    { title = doc.title
    , body =
        doc.body
            |> List.map (Html.map globalEvent)
    }


{-| -}
init : shared -> Procedure shared global local -> ( Model shared global local, Cmd (Msg global local) )
init shared (Procedure proc) =
    let
        thread =
            Internal.fromProcedure proc

        initialState =
            Internal.initialState shared

        cued =
            Internal.cue initialState thread
    in
    ( Model
        { thread = cued.next
        , state = cued.newState
        }
    , batchLocalCmds cued.cmds
    )


{-| -}
type Msg global local
    = Msg (Internal.Msg global local)


globalEvent : global -> Msg global local
globalEvent global =
    Msg (Internal.globalEvent global)


{-| -}
onUrlRequest : (Browser.UrlRequest -> global) -> Browser.UrlRequest -> Msg global local
onUrlRequest f req =
    globalEvent <| f req


{-| -}
onUrlChange : (Url -> global) -> Url -> Msg global local
onUrlChange f url =
    globalEvent <| f url


{-| -}
update : Msg global local -> Model shared global local -> ( Model shared global local, Cmd (Msg global local) )
update (Msg msg) (Model model) =
    let
        res =
            Internal.runWithMsg msg model.state model.thread
    in
    ( Model
        { thread = res.next
        , state = res.newState
        }
    , batchLocalCmds res.cmds
    )


batchLocalCmds : List ( ThreadId, Cmd local ) -> Cmd (Msg global local)
batchLocalCmds cmds =
    cmds
        |> List.map (\( tid, cmd ) -> Cmd.map (Msg << Internal.threadEvent tid) cmd)
        |> Cmd.batch


{-| -}
subscriptions : (shared -> Sub global) -> Model shared global local -> Sub (Msg global local)
subscriptions f (Model model) =
    f model.state.shared
        |> Sub.map globalEvent


{-| Procedures to be processed in a thread.

  - shared: Shared memory
  - global: Global events
  - local: Local events that only affect on the forked thread itself

-}
type Procedure shared global local
    = Procedure (Internal.Procedure (Cmd local) shared global local)


{-| Construct a `Procedure` instance that do nothing.
-}
none : Procedure shared global local
none =
    Procedure Internal.none


{-| Batch `Procedure`s together. The elements are evaluated in order.
-}
batch : List (Procedure shared global local) -> Procedure shared global local
batch procs =
    List.map (\(Procedure proc) -> proc) procs
        |> Internal.batch
        |> Procedure


{-| Construct a `Procedure` instance that modifies shared memory state.
-}
modify : (shared -> shared) -> Procedure shared global local
modify f =
    Procedure <| Internal.modify f


{-| Construct a `Procedure` instance that pushes a local `Cmd`.
-}
push : (shared -> Cmd local) -> Procedure shared global local
push f =
    Procedure <| Internal.push (f >> List.singleton)


{-| Construct a `Procedure` instance that awaits the local events for the thread.

If it returns `Nothing`, it awaits again.
Otherwise, it evaluates the given `Procedure`.

-}
await : (local -> shared -> Maybe (Procedure shared global local)) -> Procedure shared global local
await f =
    Procedure <|
        Internal.await <|
            \local shared ->
                f local shared
                    |> Maybe.map (\(Procedure proc) -> proc)


{-| Construct a `Procedure` instance that awaits global events.

If it returns `Nothing`, it awaits again.
Otherwise, it evaluates the given `Procedure`.

-}
awaitGlobal : (global -> shared -> Maybe (Procedure shared global local)) -> Procedure shared global local
awaitGlobal f =
    Procedure <|
        Internal.awaitGlobal <|
            \global shared ->
                f global shared
                    |> Maybe.map (\(Procedure proc) -> proc)


{-| Construct a `Procedure` instance that evaluates the given `Procedure` in a forked thread.

The forked thread runs independently of the original thread.
i.e., The subsequent `Procedure`s in the original thread are evaluated immediately.

-}
fork : (() -> Procedure shared global local) -> Procedure shared global local
fork f =
    Procedure <|
        Internal.fork <|
            \a ->
                f a
                    |> (\(Procedure proc) -> proc)


{-| Construct a `Procedure` instance that wait for all given `Procedure`s to be completed.

Given `Procedure`s are evaluated in the independent thread, but the subsequent `Procedure`s in the original thread are **not** evaluated immediately.

-}
syncAll : List (Procedure shared global local) -> Procedure shared global local
syncAll procs =
    List.map (\(Procedure proc) -> proc) procs
        |> Internal.syncAll
        |> Procedure


{-| Quit the thread immediately.

Subsequent `Procedures` are not evaluated and are discarded.

-}
quit : Procedure shared global local
quit =
    Procedure Internal.quit
