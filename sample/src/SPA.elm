module SPA exposing (..)

import SPA.Page.Home as Home
import SPA.Page.Users as Users
import Thread.Lifter exposing (Lifter)
import Thread.Procedure as Procedure exposing (Procedure)
import Thread.Wrapper exposing (Wrapper)



-- Shared


type alias Shared =
    { home : Home.Shared
    , users : Users.Shared
    }


init : Shared
init =
    { home = Home.init
    , users = Users.init
    }


homeLifter : Lifter Shared Home.Shared
homeLifter =
    { get = .home
    , set = \home shared -> { shared | home = home }
    }


usersLifter : Lifter Shared Users.Shared
usersLifter =
    { get = .users
    , set = \users shared -> { shared | users = users }
    }



-- Global


type Global
    = GlobalEvent1
    | HomeGlobal Home.Global
    | UsersGlobal Users.Global


unwrapHomeGlobal : Global -> Maybe Home.Global
unwrapHomeGlobal global =
    case global of
        HomeGlobal home ->
            Just home

        _ ->
            Nothing


unwrapUsersGlobal : Global -> Maybe Users.Global
unwrapUsersGlobal global =
    case global of
        UsersGlobal users ->
            Just users

        _ ->
            Nothing



-- Local


type Local
    = LocalEvent1
    | HomeLocal Home.Local
    | UsersLocal Users.Local


unwrapHomeLocal : Local -> Maybe Home.Local
unwrapHomeLocal local =
    case local of
        HomeLocal home ->
            Just home

        _ ->
            Nothing


unwrapUsersLocal : Local -> Maybe Users.Local
unwrapUsersLocal local =
    case local of
        UsersLocal users ->
            Just users

        _ ->
            Nothing



-- Procedure


procedure : Procedure Shared Global Local
procedure =
    Procedure.batch
        [ Procedure.fork <|
            \() ->
                Home.procedure
                    |> Procedure.liftShared homeLifter
                    |> Procedure.wrapGlobal unwrapHomeGlobal
                    |> Procedure.wrapLocal
                        { wrap = HomeLocal
                        , unwrap = unwrapHomeLocal
                        }
        , Procedure.fork <|
            \() ->
                Users.procedure
                    |> Procedure.liftShared usersLifter
                    |> Procedure.wrapGlobal unwrapUsersGlobal
                    |> Procedure.wrapLocal
                        { wrap = UsersLocal
                        , unwrap = unwrapUsersLocal
                        }
        , Debug.todo "subsequent procedures..."
        ]
