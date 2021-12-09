module SPA.Page.Home exposing
    ( Event
    , Memory
    , init
    , procedures
    )

import Thread.Procedure exposing (Block)


type Memory
    = Memory


type Event
    = Event


init : Memory
init =
    Memory


procedures : Block Memory Event
procedures =
    Debug.todo ""
