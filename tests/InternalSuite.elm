module InternalSuite exposing (suite)

import Expect
import Internal.Test exposing (..)
import Procedure
import Procedure.ObserverId exposing (ObserverId)
import Test exposing (Test)
import Test.Sequence as Sequence


suite : Test
suite =
    Sequence.describe "simulate SPA"
        |> Sequence.map
            (\_ ->
                Procedure.init init <| procedures (Url "/foo")
            )
        |> Sequence.assert "NotFound page"
            (\( model, cmds ) ->
                ( Procedure.memoryState model
                , cmds
                )
                    |> Expect.equal
                        ( { page = PageNotFound }
                        , [ DisplayLog "pageController"
                          ]
                        )
            )
        |> Sequence.map
            (\_ ->
                Procedure.init init <| procedures (Url "/")
            )
        |> Sequence.assert "load page"
            (\( model, cmds ) ->
                ( Procedure.memoryState model
                , List.head cmds
                )
                    |> Expect.equal
                        ( { page = PageLoading
                          }
                        , Just <| DisplayLog "pageController"
                        )
            )
        |> Sequence.andThen "issue `RequestSession` cmd."
            (\( model, cmds ) ->
                List.filterMap
                    (\cmd ->
                        case cmd of
                            RequestSession localOid ->
                                Just localOid

                            _ ->
                                Nothing
                    )
                    cmds
                    |> List.head
                    |> Maybe.map
                        (\oid ->
                            { loadPage = model
                            , local = oid
                            }
                        )
            )
        |> Sequence.assert "failed to fetch session"
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.issue
                                o.local
                                (ReceiveSession (Err (SomeError "failedToFetchSession")))
                            )
                            o.loadPage
                in
                { memory = Procedure.memoryState model
                , cmds = cmds
                }
                    |> Expect.equal
                        { memory =
                            { page = PageLoading
                            }
                        , cmds =
                            [ DisplayError "failedToFetchSession"
                            ]
                        }
            )
        |> Sequence.assert "already logged in"
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.issue
                                o.local
                                (ReceiveSession (Ok { name = "hello" }))
                            )
                            o.loadPage
                in
                { memory =
                    Procedure.memoryState model
                        |> unwrapPageHome
                , cmds = cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "hello"
                                    }
                                }
                        , cmds =
                            []
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.issue
                                o.local
                                (ReceiveSession (Err LoginRequired))
                            )
                            o.loadPage
                in
                { loginRequired = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "login required"
            (\o ->
                { memory = Procedure.memoryState o.loginRequired
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            { page = PageLoading
                            }
                        , cmds =
                            [ PushRoute <| RouteLogin { back = RouteHome }
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.publish
                                (UrlChanged <| encodeUrl <| RouteLogin { back = RouteHome })
                            )
                            o.loginRequired
                in
                { pushRouteLogin = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "push Route.Login"
            (\o ->
                { memory = unwrapPageLogin <| Procedure.memoryState o.pushRouteLogin
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory = Just initPageLogin
                        , cmds =
                            [ DisplayLog "pageController"
                            ]
                        }
            )
        |> Sequence.andThen "should have ObserverId for PageLogin"
            (\o ->
                case (Procedure.memoryState o.pushRouteLogin).page of
                    PageLogin ( oid, _ ) ->
                        Just
                            { pushRouteLogin = o.pushRouteLogin
                            , pageLoginOid = oid
                            }

                    _ ->
                        Nothing
            )
        |> Sequence.assert "cancel login with external link"
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.publish
                                (LinkClicked <| External "https://example.com/foo")
                            )
                            o.pushRouteLogin
                in
                { memory = unwrapPageLogin <| Procedure.memoryState model
                , cmds = cmds
                }
                    |> Expect.equal
                        { memory = Just initPageLogin
                        , cmds =
                            [ DisplayLog "Clicked external link: https://example.com/foo"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        Procedure.update
                            (Procedure.publish
                                (LinkClicked <| Internal <| Url "/")
                            )
                            o.pushRouteLogin
                in
                { pushRouteLogin = o.pushRouteLogin
                , pageLoginOid = o.pageLoginOid
                , cancelLogin = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "cancel login"
            (\o ->
                { memory = unwrapPageLogin <| Procedure.memoryState o.cancelLogin
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory = Just initPageLogin
                        , cmds =
                            [ DisplayLog "Clicked internal link: /"
                            ]
                        }
            )
        |> Sequence.assert "cancel login2"
            (\o ->
                let
                    ( memory, cmds ) =
                        Procedure.update
                            (Procedure.publish
                                (UrlChanged <| encodeUrl <| RouteHome)
                            )
                            o.cancelLogin
                in
                { memory = unwrapPageLogin <| Procedure.memoryState memory
                , cmds =
                    cmds
                        |> List.map
                            (\cmd ->
                                case cmd of
                                    DisplayLog str ->
                                        "DisplayLog: " ++ str

                                    RequestSession _ ->
                                        "RequestSession"

                                    _ ->
                                        ""
                            )
                }
                    |> Expect.equal
                        { memory = Just initPageLogin
                        , cmds =
                            [ "DisplayLog: pageController"
                            , "RequestSession"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.pushRouteLogin
                            |> Procedure.update
                                (Procedure.issue o.pageLoginOid
                                    (PageLoginChangePass "pass")
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue o.pageLoginOid
                                    (PageLoginChangeId "id")
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue o.pageLoginOid
                                    PageLoginClickSubmit
                                )
                in
                { pageLoginOid = o.pageLoginOid
                , pageLoginSubmit = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "PageLogin submit"
            (\o ->
                { memory = unwrapPageLogin <| Procedure.memoryState o.pageLoginSubmit
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { id = "id"
                                , pass = "pass"
                                , msession = Nothing
                                }
                        , cmds =
                            [ PageLoginRequestSubmit
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageLoginSubmit
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageLoginOid
                                    (PageLoginReceiveLoginResp (Err (SomeError "Invalid ID or Pass")))
                                )
                in
                { pageLoginOid = o.pageLoginOid
                , pageLoginAuthError = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "PageLogin auth error"
            (\o ->
                { memory = unwrapPageLogin <| Procedure.memoryState o.pageLoginAuthError
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { id = "id"
                                , pass = "pass"
                                , msession = Nothing
                                }
                        , cmds =
                            [ DisplayError "Invalid ID or Pass"
                            ]
                        }
            )
        |> Sequence.andThen "should have ObserverId for PageHome"
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageLoginAuthError
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageLoginOid
                                    (PageLoginChangePass "pass2")
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageLoginOid
                                    PageLoginClickSubmit
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageLoginOid
                                    (PageLoginReceiveLoginResp <|
                                        Ok
                                            { session =
                                                { name = "Sakura-chan" }
                                            }
                                    )
                                )
                in
                case (Procedure.memoryState model).page of
                    PageHome ( oid, _ ) ->
                        Just
                            { pageHomeOid = oid
                            , pageLoginRetry = model
                            , cmds = cmds
                            }

                    _ ->
                        Nothing
            )
        |> Sequence.assert "PageLogin retry"
            (\o ->
                { memory = unwrapPageHome <| Procedure.memoryState o.pageLoginRetry
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "Sakura-chan"
                                    }
                                }
                        , cmds =
                            [ DisplayLog "pageController"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageLoginRetry
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageHomeOid
                                    (PageHomeEvent PageHomeClickButton1)
                                )
                in
                { pageHomeOid = o.pageHomeOid
                , pageHomeClickButton1 = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "Page.Home click button1"
            (\o ->
                { memory = unwrapPageHome <| Procedure.memoryState o.pageHomeClickButton1
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "Sakura-chan"
                                    }
                                }
                        , cmds =
                            [ PageHomeCmd <| PageHomeDisplayLog <| "[Page.Home] ClickButton1"
                            , PageHomeCmd <| PageHomeDisplayLog <| "[Page.Home] ClickButton1 in another thread"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageHomeClickButton1
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageHomeOid
                                    (PageHomeEvent PageHomeClickButton2)
                                )
                in
                { pageHomeOid = o.pageHomeOid
                , pageHomeClickButton2 = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "Page.Home click button2"
            (\o ->
                { memory = unwrapPageHome <| Procedure.memoryState o.pageHomeClickButton2
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "Sakura-chan"
                                    }
                                }
                        , cmds =
                            [ PageHomeCmd <| PageHomeDisplayLog <| "[Page.Home] ClickButton2 in asynced thread"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageHomeClickButton2
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageHomeOid
                                    (PageHomeEvent <| PageHomeChangeSessionName "new Sakura-chan")
                                )
                in
                { pageHomeOid = o.pageHomeOid
                , pageHomeChangeSessionName = model
                , cmds = cmds
                }
            )
        |> Sequence.assert "Page.Home change session name"
            (\o ->
                { memory = unwrapPageHome <| Procedure.memoryState o.pageHomeChangeSessionName
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                }
                        , cmds =
                            [ PageHomeCmd <| PageHomeDisplayLog <| "[Page.Home] Initial \"name\" session: Sakura-chan"
                            , PageHomeCmd <| PageHomeDisplayLog <| "[Page.Home] Released initial \"name\" session: Sakura-chan"
                            ]
                        }
            )
        |> Sequence.assert "No effect on another observer"
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageHomeChangeSessionName
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageHomeOid
                                    (UrlChanged <| encodeUrl <| RouteUsers)
                                )
                in
                { memory = unwrapPageHome <| Procedure.memoryState model
                , cmds = cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                }
                        , cmds =
                            []
                        }
            )
        |> Sequence.andThen "should have ObserverId for PageUsers"
            (\o ->
                let
                    ( model, cmds ) =
                        o.pageHomeChangeSessionName
                            |> Procedure.update
                                (Procedure.publish
                                    (UrlChanged <| encodeUrl <| RouteUsers)
                                )
                in
                case (Procedure.memoryState model).page of
                    PageUsers ( oid, _ ) ->
                        Just
                            { gotoPageUsers = model
                            , cmds = cmds
                            , pageUsersOid = oid
                            }

                    _ ->
                        Nothing
            )
        |> Sequence.assert "Go to PageUsers"
            (\o ->
                { memory = unwrapPageUsers <| Procedure.memoryState o.gotoPageUsers
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                , users = []
                                }
                        , cmds =
                            [ PageUsersCmd PageUsersRequestInitialUsers
                            ]
                        }
            )
        |> Sequence.andThen "should have ObserverId for user3"
            (\o ->
                let
                    ( model, cmds ) =
                        o.gotoPageUsers
                            |> Procedure.update
                                (Procedure.issue
                                    o.pageUsersOid
                                    (PageUsersEvent <|
                                        PageUsersReceiveInitialUsers <|
                                            Ok
                                                [ user1, user2, user3, user4 ]
                                    )
                                )
                in
                unwrapPageUsersUserOid user3.name (Procedure.memoryState model)
                    |> Maybe.map
                        (\oid ->
                            { fetchInitialUsers = model
                            , cmds = cmds
                            , pageUsersOid = o.pageUsersOid
                            , user3Oid = oid
                            }
                        )
            )
        |> Sequence.assert "fetch initial users"
            (\o ->
                { memory = unwrapPageUsers <| Procedure.memoryState o.fetchInitialUsers
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                , users =
                                    [ initUserForm user1
                                    , initUserForm user2
                                    , initUserForm user3
                                    , initUserForm user4
                                    ]
                                }
                        , cmds =
                            [ PageUsersCmd <| PageUsersDisplayLog "[Page.Users] Loaded users"
                            ]
                        }
            )
        |> Sequence.map
            (\o ->
                let
                    ( model, cmds ) =
                        o.fetchInitialUsers
                            |> Procedure.update
                                (Procedure.issue
                                    o.user3Oid
                                    (PageUsersEvent <| PageUsersChangeNewUserName user5.name)
                                )
                in
                { user3ChangeNewUserName = model
                , cmds = cmds
                , user3Oid = o.user3Oid
                }
            )
        |> Sequence.assert "Change new user name on User 3"
            (\o ->
                { memory = unwrapPageUsers <| Procedure.memoryState o.user3ChangeNewUserName
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                , users =
                                    [ initUserForm user1
                                    , initUserForm user2
                                    , { user = user3
                                      , newUserName = user5.name
                                      }
                                    , initUserForm user4
                                    ]
                                }
                        , cmds =
                            []
                        }
            )
        |> Sequence.andThen "should have ObserverId for user5"
            (\o ->
                let
                    ( model, cmds ) =
                        o.user3ChangeNewUserName
                            |> Procedure.update
                                (Procedure.issue
                                    o.user3Oid
                                    (PageUsersEvent <| PageUsersClickRegisterNewUser)
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue
                                    o.user3Oid
                                    (PageUsersEvent <| PageUsersReceiveRegisterNewUserResp <| Ok user5)
                                )
                in
                unwrapPageUsersUserOid user5.name (Procedure.memoryState model)
                    |> Maybe.map
                        (\oid ->
                            { user3ClickRegisterNewUser = model
                            , cmds = cmds
                            , user5Oid = oid
                            }
                        )
            )
        |> Sequence.assert "Click register new user on User 3"
            (\o ->
                { memory = unwrapPageUsers <| Procedure.memoryState o.user3ClickRegisterNewUser
                , cmds = o.cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                , users =
                                    [ initUserForm user1
                                    , initUserForm user2
                                    , initUserForm user3
                                    , initUserForm user5
                                    , initUserForm user4
                                    ]
                                }
                        , cmds =
                            []
                        }
            )
        |> Sequence.assert "Click register new user on User 5"
            (\o ->
                let
                    ( model, cmds ) =
                        o.user3ClickRegisterNewUser
                            |> Procedure.update
                                (Procedure.issue
                                    o.user5Oid
                                    (PageUsersEvent <| PageUsersChangeNewUserName user6.name)
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue
                                    o.user5Oid
                                    (PageUsersEvent <| PageUsersClickRegisterNewUser)
                                )
                            |> Tuple.first
                            |> Procedure.update
                                (Procedure.issue
                                    o.user5Oid
                                    (PageUsersEvent <| PageUsersReceiveRegisterNewUserResp <| Ok user6)
                                )
                in
                { memory = unwrapPageUsers <| Procedure.memoryState model
                , cmds = cmds
                }
                    |> Expect.equal
                        { memory =
                            Just
                                { session =
                                    { name = "new Sakura-chan"
                                    }
                                , users =
                                    [ initUserForm user1
                                    , initUserForm user2
                                    , initUserForm user3
                                    , initUserForm user5
                                    , initUserForm user6
                                    , initUserForm user4
                                    ]
                                }
                        , cmds =
                            []
                        }
            )
        |> Sequence.run


unwrapPageHome : Memory -> Maybe PageHomeMemory
unwrapPageHome memory =
    case memory.page of
        PageHome ( _, a ) ->
            Just a

        _ ->
            Nothing


unwrapPageUsers : Memory -> Maybe { session : Session, users : List UserForm }
unwrapPageUsers memory =
    case memory.page of
        PageUsers ( _, a ) ->
            Just
                { session = a.session
                , users = List.map Tuple.second a.users
                }

        _ ->
            Nothing


unwrapPageLogin : Memory -> Maybe PageLogin_
unwrapPageLogin memory =
    case memory.page of
        PageLogin ( _, a ) ->
            Just a

        _ ->
            Nothing


unwrapPageUsersUserOid : String -> Memory -> Maybe ObserverId
unwrapPageUsersUserOid name memory =
    case memory.page of
        PageUsers ( _, { users } ) ->
            users
                |> List.filterMap
                    (\( oid, { user } ) ->
                        if user.name == name then
                            Just oid

                        else
                            Nothing
                    )
                |> List.head

        _ ->
            Nothing
