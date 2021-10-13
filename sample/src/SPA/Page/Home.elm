module SPA.Page.Home exposing
    ( Global
    , Local
    , Shared
    , init
    , procedure
    )

import Thread.Procedure exposing (Procedure)


type Shared
    = Shared


type Global
    = Global


type Local
    = Local


init : Shared
init =
    Shared


procedure : Procedure Shared Global Local
procedure =
    Debug.todo ""
