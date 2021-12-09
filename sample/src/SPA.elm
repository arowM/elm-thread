module SPA exposing (..)

import SPA.Page.Home as Home
import SPA.Page.Users as Users
import Thread.Lifter exposing (Lifter)
import Thread.Procedure as Procedure exposing (Block, Procedure)
import Thread.Wrapper exposing (Wrapper)



-- Memory


type alias Memory =
    { home : Home.Memory
    , users : Users.Memory
    }


init : Memory
init =
    { home = Home.init
    , users = Users.init
    }


homeLifter : Lifter Memory Home.Memory
homeLifter =
    { get = .home >> Just
    , set = \home shared -> { shared | home = home }
    }


usersLifter : Lifter Memory Users.Memory
usersLifter =
    { get = .users >> Just
    , set = \users shared -> { shared | users = users }
    }



-- Event


type Event
    = Event1
    | HomeEvent Home.Event
    | UsersEvent Users.Event


homeWrapper : Wrapper Event Home.Event
homeWrapper =
    { unwrap =
        \event ->
            case event of
                HomeEvent home ->
                    Just home

                _ ->
                    Nothing
    , wrap = HomeEvent
    }


usersWrapper : Wrapper Event Users.Event
usersWrapper =
    { unwrap =
        \event ->
            case event of
                UsersEvent users ->
                    Just users

                _ ->
                    Nothing
    , wrap = UsersEvent
    }



-- Procedure


procedures : Block Memory Event
procedures _ =
    [ Procedure.async
        (Home.procedures
            |> Procedure.liftBlock homeLifter
            |> Procedure.wrapBlock homeWrapper
        )
    , Procedure.async
        (Users.procedures
            |> Procedure.liftBlock usersLifter
            |> Procedure.wrapBlock usersWrapper
        )
    , Debug.todo "subsequent procedures..."
    ]
