module Internal.Test exposing (..)

import Procedure exposing (Observer, Procedure, global)
import Procedure.ObserverId exposing (ObserverId)
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as Query


user1 : User
user1 =
    { id = "1"
    , name = "user 1"
    }


user2 : User
user2 =
    { id = "2"
    , name = "user 2"
    }


user3 : User
user3 =
    { id = "3"
    , name = "user 3"
    }


user4 : User
user4 =
    { id = "4"
    , name = "user 4"
    }


user5 : User
user5 =
    { id = "5"
    , name = "user 5"
    }


user6 : User
user6 =
    { id = "6"
    , name = "user 6"
    }



-- Memory


type alias Memory =
    { page : PageView
    }


init : Memory
init =
    { page = PageLoading
    }


page : Observer Memory PageView
page =
    global
        |> Procedure.dig
            { get = .page
            , set = \p memory -> { memory | page = p }
            }


type PageView
    = PageLoading
    | PageNotFound
    | PageLogin ( ObserverId, PageLogin_ )
    | PageHome ( ObserverId, PageHomeMemory )
    | PageUsers ( ObserverId, PageUsersMemory )


type alias PageLogin_ =
    { id : String
    , pass : String
    , msession : Maybe Session
    }


initPageLogin : PageLogin_
initPageLogin =
    { id = ""
    , pass = ""
    , msession = Nothing
    }


type alias Session =
    { name : String
    }


putLog : String -> Procedure Cmd Memory Event
putLog str =
    Procedure.push global <| \_ _ -> DisplayLog str


putError : String -> Procedure Cmd Memory Event
putError str =
    Procedure.push global <| \_ _ -> DisplayError str



-- Events


type Event
    = LinkClicked UrlRequest
    | UrlChanged Url
    | PageHomeEvent PageHomeEvent
    | PageUsersEvent PageUsersEvent
    | ReceiveSession (Result HttpError Session)
    | PageLoginReceiveLoginResp (Result HttpError LoginResp)
    | PageLoginChangeId String
    | PageLoginChangePass String
    | PageLoginClickSubmit


type UrlRequest
    = Internal Url
    | External String


type alias LoginResp =
    { session : Session
    }


type HttpError
    = LoginRequired
    | SomeError String


type Route
    = RouteNotFound
    | RouteLogin RouteLogin_
    | RouteHome
    | RouteUsers


type Url
    = Url String


encodeUrl : Route -> Url
encodeUrl route =
    Url <|
        case route of
            RouteNotFound ->
                Url.Builder.absolute
                    [ "not-found"
                    ]
                    []

            RouteLogin detail ->
                Url.Builder.absolute
                    [ "login"
                    ]
                    [ Url.Builder.string "back" <|
                        let
                            (Url back) =
                                encodeUrl detail.back
                        in
                        back
                    ]

            RouteHome ->
                Url.Builder.absolute
                    []
                    []

            RouteUsers ->
                Url.Builder.absolute
                    [ "users"
                    ]
                    []


type alias RouteLogin_ =
    { back : Route
    }


decodeUrl : Url -> Route
decodeUrl (Url url) =
    Url.fromString ("https://example.com" ++ url)
        |> Maybe.andThen
            (Url.Parser.parse urlParser)
        |> Maybe.withDefault RouteNotFound


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.s "login"
            <?> Query.string "back"
            |> Url.Parser.map
                (\mstr ->
                    RouteLogin
                        { back =
                            mstr
                                |> Maybe.map Url
                                |> Maybe.map decodeUrl
                                |> Maybe.withDefault RouteHome
                        }
                )
        , Url.Parser.top
            |> Url.Parser.map RouteHome
        , Url.Parser.s "users"
            |> Url.Parser.map RouteUsers
        ]


type Cmd
    = RequestSession ObserverId
    | PushRoute Route
    | DisplayLog String
    | DisplayError String
    | PageHomeCmd PageHomeCmd
    | PageUsersCmd PageUsersCmd
    | PageLoginRequestSubmit



-- Sample procedure


procedures : Url -> List (Procedure Cmd Memory Event)
procedures url =
    [ Procedure.async <| linkController
    , Procedure.async <| pageController (decodeUrl url) Nothing
    ]


linkController : List (Procedure Cmd Memory Event)
linkController =
    [ Procedure.await global <|
        \event _ ->
            case event of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Internal (Url url) ->
                            [ putLog <|
                                "Clicked internal link: "
                                    ++ url
                            ]

                        External href ->
                            [ putLog <|
                                "Clicked external link: "
                                    ++ href
                            ]

                _ ->
                    []
    , Procedure.jump global <| \_ -> linkController
    ]


pageController : Route -> Maybe Session -> List (Procedure Cmd Memory Event)
pageController route msession =
    let
        handleNewRoute event _ =
            case event of
                UrlChanged url ->
                    [ Procedure.jump global <| \_ -> pageController (decodeUrl url) msession
                    ]

                _ ->
                    []
    in
    [ putLog "pageController"
    , Procedure.batch <|
        case route of
            RouteNotFound ->
                [ Procedure.modify global <|
                    \memory -> { memory | page = PageNotFound }
                , Procedure.await global handleNewRoute
                ]

            RouteLogin detail ->
                [ Procedure.setVariant
                    page
                    { unwrap = unwrapPageLogin
                    , wrap = PageLogin
                    }
                    initPageLogin
                  <|
                    \pageLogin ->
                        [ Procedure.race
                            [ pageLoginProcedures pageLogin detail
                                |> Procedure.batch
                                |> assertNoGlobalPollutionOnPageLoginEvents
                            , Procedure.await global handleNewRoute
                            ]
                        , Procedure.jump pageLogin <|
                            \memory ->
                                pageController detail.back memory.msession
                        ]
                ]

            _ ->
                case msession of
                    Nothing ->
                        [ assertNoGlobalPollutionOnRequestSession <|
                            Procedure.protected global <|
                                \local ->
                                    [ Procedure.push local <| \oid _ -> RequestSession oid
                                    , Procedure.await local <|
                                        \event _ ->
                                            case event of
                                                ReceiveSession (Err LoginRequired) ->
                                                    [ Procedure.push global <|
                                                        \_ _ ->
                                                            PushRoute <| RouteLogin { back = route }
                                                    , Procedure.await global handleNewRoute
                                                    ]

                                                ReceiveSession (Err err) ->
                                                    [ handleHttpError route err
                                                    , Procedure.await global handleNewRoute
                                                    ]

                                                ReceiveSession (Ok session) ->
                                                    [ Procedure.jump global <| \_ -> sessionPageController route session
                                                    ]

                                                _ ->
                                                    []
                                    ]
                        ]

                    Just session ->
                        [ Procedure.jump global <| \_ -> sessionPageController route session
                        ]
    , Procedure.quit
    , putError "`quit` should quit the thread."
    ]


handleHttpError : Route -> HttpError -> Procedure Cmd Memory Event
handleHttpError route err =
    Procedure.batch <|
        case err of
            LoginRequired ->
                [ Procedure.push global <|
                    \_ _ ->
                        PushRoute <| RouteLogin { back = route }
                ]

            SomeError str ->
                [ putError str
                ]


assertNoGlobalPollutionOnPageLoginEvents : Procedure Cmd Memory Event -> Procedure Cmd Memory Event
assertNoGlobalPollutionOnPageLoginEvents p =
    Procedure.race
        [ Procedure.await global <|
            \event _ ->
                case event of
                    PageLoginReceiveLoginResp _ ->
                        [ putError "Global pollution with PageLoginReceiveLoginResp"
                        ]

                    PageLoginChangeId _ ->
                        [ putError "Global pollution with PageLoginChangeId"
                        ]

                    PageLoginChangePass _ ->
                        [ putError "Global pollution with PageLoginChangePass"
                        ]

                    PageLoginClickSubmit ->
                        [ putError "Global pollution with PageLoginClickSubmit"
                        ]

                    _ ->
                        []
        , p
        ]


assertNoGlobalPollutionOnPageHomeEvents : Procedure Cmd Memory Event -> Procedure Cmd Memory Event
assertNoGlobalPollutionOnPageHomeEvents p =
    Procedure.race
        [ Procedure.await global <|
            \event _ ->
                case event of
                    PageHomeEvent _ ->
                        [ putError "Global pollution with PageHomeEvent"
                        ]

                    _ ->
                        []
        , p
        ]


assertNoGlobalPollutionOnPageUsersEvents : Procedure Cmd Memory Event -> Procedure Cmd Memory Event
assertNoGlobalPollutionOnPageUsersEvents p =
    Procedure.race
        [ Procedure.await global <|
            \event _ ->
                case event of
                    PageUsersEvent _ ->
                        [ putError "Global pollution with PageUsersEvent"
                        ]

                    _ ->
                        []
        , p
        ]


assertNoGlobalPollutionOnRequestSession : Procedure Cmd Memory Event -> Procedure Cmd Memory Event
assertNoGlobalPollutionOnRequestSession p =
    Procedure.race
        [ Procedure.await global <|
            \event _ ->
                case event of
                    ReceiveSession _ ->
                        [ putError "Global pollution on Request session"
                        ]

                    _ ->
                        []
        , p
        ]


pageLoginProcedures : Observer Memory PageLogin_ -> RouteLogin_ -> List (Procedure Cmd Memory Event)
pageLoginProcedures pageLogin detail =
    [ Procedure.await pageLogin <|
        \event _ ->
            case event of
                PageLoginChangeId str ->
                    [ Procedure.modify pageLogin <|
                        \memory -> { memory | id = str }
                    , Procedure.jump global <| \_ -> pageLoginProcedures pageLogin detail
                    ]

                PageLoginChangePass str ->
                    [ Procedure.modify pageLogin <|
                        \memory -> { memory | pass = str }
                    , Procedure.jump global <| \_ -> pageLoginProcedures pageLogin detail
                    ]

                PageLoginClickSubmit ->
                    [ Procedure.push pageLogin <|
                        \_ _ -> PageLoginRequestSubmit
                    , Procedure.await pageLogin <|
                        \event2 _ ->
                            case event2 of
                                PageLoginReceiveLoginResp (Err LoginRequired) ->
                                    [ putError "Something went wrong."
                                    , Procedure.jump global <| \_ -> pageLoginProcedures pageLogin detail
                                    ]

                                PageLoginReceiveLoginResp (Err err) ->
                                    [ handleHttpError detail.back err
                                    , Procedure.jump global <| \_ -> pageLoginProcedures pageLogin detail
                                    ]

                                PageLoginReceiveLoginResp (Ok resp) ->
                                    [ Procedure.modify pageLogin <|
                                        \memory ->
                                            { memory | msession = Just resp.session }
                                    , Procedure.quit
                                    ]

                                _ ->
                                    []
                    ]

                _ ->
                    []
    , putError "`jump` and `quit` should ignore subsequent procedures."
    ]


sessionPageController : Route -> Session -> List (Procedure Cmd Memory Event)
sessionPageController route session =
    let
        handleNewRoute event memory =
            case event of
                UrlChanged url ->
                    [ Procedure.jump global <| \_ -> sessionPageController (decodeUrl url) (extractSession session memory.page)
                    ]

                _ ->
                    []
    in
    case route of
        RouteNotFound ->
            [ Procedure.modify global <|
                \memory -> { memory | page = PageNotFound }
            , Procedure.await global handleNewRoute
            ]

        RouteLogin detail ->
            [ Procedure.setVariant
                page
                { unwrap = unwrapPageLogin
                , wrap = PageLogin
                }
                initPageLogin
              <|
                \pageLogin ->
                    [ Procedure.doUntil global
                        (pageLoginProcedures pageLogin detail)
                        handleNewRoute
                        |> assertNoGlobalPollutionOnPageLoginEvents
                    ]
            ]

        RouteHome ->
            [ Procedure.setVariant
                page
                { unwrap = unwrapPageHome
                , wrap = PageHome
                }
                (initPageHome session)
              <|
                \pageHome ->
                    [ Procedure.doUntil global
                        (pageHomeProcedures
                            PageHomeCmd
                            PageHomeEvent
                            unwrapPageHomeEvent
                            pageHome
                        )
                        handleNewRoute
                        |> assertNoGlobalPollutionOnPageHomeEvents
                    ]
            ]

        RouteUsers ->
            [ Procedure.setVariant
                page
                { unwrap = unwrapPageUsers
                , wrap = PageUsers
                }
                (initPageUsers session)
              <|
                \pageUsers ->
                    [ Procedure.doUntil global
                        (pageUsersProcedures
                            PageUsersCmd
                            PageUsersEvent
                            unwrapPageUsersEvent
                            pageUsers
                        )
                        handleNewRoute
                        |> assertNoGlobalPollutionOnPageUsersEvents
                    ]
            ]


unwrapPageLogin : PageView -> Maybe ( ObserverId, PageLogin_ )
unwrapPageLogin pv =
    case pv of
        PageLogin a ->
            Just a

        _ ->
            Nothing


unwrapPageHome : PageView -> Maybe ( ObserverId, PageHomeMemory )
unwrapPageHome pv =
    case pv of
        PageHome a ->
            Just a

        _ ->
            Nothing


unwrapPageUsers : PageView -> Maybe ( ObserverId, PageUsersMemory )
unwrapPageUsers pv =
    case pv of
        PageUsers a ->
            Just a

        _ ->
            Nothing


extractSession : Session -> PageView -> Session
extractSession def pv =
    case pv of
        PageLoading ->
            def

        PageNotFound ->
            def

        PageLogin ( _, detail ) ->
            Maybe.withDefault def detail.msession

        PageHome ( _, detail ) ->
            detail.session

        PageUsers ( _, detail ) ->
            detail.session


unwrapPageHomeEvent : Event -> Maybe PageHomeEvent
unwrapPageHomeEvent event =
    case event of
        PageHomeEvent content ->
            Just content

        _ ->
            Nothing


unwrapPageUsersEvent : Event -> Maybe PageUsersEvent
unwrapPageUsersEvent event =
    case event of
        PageUsersEvent content ->
            Just content

        _ ->
            Nothing



-- Page.Home


type alias PageHomeMemory =
    { session : Session
    }


initPageHome : Session -> PageHomeMemory
initPageHome session =
    { session = session
    }


type PageHomeEvent
    = PageHomeChangeSessionName String
    | PageHomeClickButton1
    | PageHomeClickButton2


type PageHomeCmd
    = PageHomeDisplayLog String


pageHomeProcedures :
    (PageHomeCmd -> cmd)
    -> (PageHomeEvent -> event)
    -> (event -> Maybe PageHomeEvent)
    -> Observer memory PageHomeMemory
    -> List (Procedure cmd memory event)
pageHomeProcedures toCmd toEvent unwrapEvent pageHome =
    [ Procedure.withResource pageHome
        { aquire = \_ memory -> ( memory, memory.session.name )
        , release =
            \name memory ->
                ( memory
                , [ toCmd <| PageHomeDisplayLog <| "[Page.Home] Released initial \"name\" session: " ++ name ]
                )
        }
      <|
        \name ->
            [ Procedure.sync
                [ Procedure.race
                    [ Procedure.await pageHome <|
                        \event _ ->
                            case unwrapEvent event of
                                Just PageHomeClickButton1 ->
                                    [ putPageHomeLog toCmd pageHome "ClickButton1" ]

                                _ ->
                                    []
                    , Procedure.await pageHome <| \_ _ -> []
                    ]
                , Procedure.batch
                    [ Procedure.async
                        [ Procedure.await pageHome <|
                            \event _ ->
                                case unwrapEvent event of
                                    Just PageHomeClickButton2 ->
                                        [ putPageHomeLog toCmd pageHome "ClickButton2 in asynced thread" ]

                                    _ ->
                                        []
                        ]
                    , Procedure.await pageHome <|
                        \event _ ->
                            case unwrapEvent event of
                                Just PageHomeClickButton1 ->
                                    [ putPageHomeLog toCmd pageHome "ClickButton1 in another thread" ]

                                _ ->
                                    []
                    ]
                ]
            , Procedure.await pageHome <|
                \event _ ->
                    case unwrapEvent event of
                        Just (PageHomeChangeSessionName str) ->
                            [ Procedure.modify pageHome <|
                                \memory ->
                                    { memory
                                        | session =
                                            let
                                                session =
                                                    memory.session
                                            in
                                            { session | name = str }
                                    }
                            ]

                        _ ->
                            []
            , putPageHomeLog toCmd pageHome <| "Initial \"name\" session: " ++ name
            ]
    , Procedure.jump global <|
        \_ ->
            pageHomeProcedures toCmd toEvent unwrapEvent pageHome
    ]


putPageHomeLog : (PageHomeCmd -> cmd) -> Observer memory PageHomeMemory -> String -> Procedure cmd memory event
putPageHomeLog toCmd pageHome str =
    Procedure.push pageHome <|
        \_ _ ->
            toCmd <| PageHomeDisplayLog <| "[Page.Home] " ++ str



-- Page.Users


type alias PageUsersMemory =
    { session : Session
    , users : List ( ObserverId, UserForm )
    }


initPageUsers : Session -> PageUsersMemory
initPageUsers session =
    { session = session
    , users = []
    }


type alias UserForm =
    { user : User
    , newUserName : String
    }


initUserForm : User -> UserForm
initUserForm user =
    { user = user
    , newUserName = ""
    }


type alias User =
    { id : String
    , name : String
    }


type PageUsersEvent
    = PageUsersReceiveInitialUsers (Result HttpError (List User))
    | PageUsersChangeNewUserName String
    | PageUsersClickRegisterNewUser
    | PageUsersReceiveRegisterNewUserResp (Result HttpError User)


type PageUsersCmd
    = PageUsersPushRoute Route
    | PageUsersRequestInitialUsers
    | PageUsersRequestRegisterNewUser
    | PageUsersDisplayLog String


pageUsersProcedures :
    (PageUsersCmd -> cmd)
    -> (PageUsersEvent -> event)
    -> (event -> Maybe PageUsersEvent)
    -> Observer memory PageUsersMemory
    -> List (Procedure cmd memory event)
pageUsersProcedures toCmd toEvent unwrapEvent pageUsers =
    let
        users =
            pageUsers
                |> Procedure.dig
                    { get = .users
                    , set = \u memory -> { memory | users = u }
                    }
    in
    [ Procedure.push pageUsers <|
        \_ _ -> toCmd PageUsersRequestInitialUsers
    , Procedure.await pageUsers <|
        \event _ ->
            case unwrapEvent event of
                Just (PageUsersReceiveInitialUsers (Err err)) ->
                    [ handlePageUsersHttpError toCmd pageUsers RouteUsers err
                    , Procedure.quit
                    ]

                Just (PageUsersReceiveInitialUsers (Ok resp)) ->
                    List.map
                        (\user ->
                            Procedure.async
                                [ Procedure.append users (initUserForm user) <|
                                    pageUsersUserFormProcedures toCmd toEvent unwrapEvent pageUsers
                                ]
                        )
                        resp

                _ ->
                    []
    , putPageUsersLog toCmd pageUsers "Loaded users"
    ]


pageUsersUserFormProcedures :
    (PageUsersCmd -> cmd)
    -> (PageUsersEvent -> event)
    -> (event -> Maybe PageUsersEvent)
    -> Observer memory PageUsersMemory
    -> Observer memory UserForm
    -> List (Procedure cmd memory event)
pageUsersUserFormProcedures toCmd toEvent unwrapEvent pageUsers userForm =
    let
        users =
            pageUsers
                |> Procedure.dig
                    { get = .users
                    , set = \u memory -> { memory | users = u }
                    }
    in
    [ Procedure.await userForm <|
        \event _ ->
            case unwrapEvent event of
                Just (PageUsersChangeNewUserName name) ->
                    [ Procedure.modify userForm <|
                        \memory -> { memory | newUserName = name }
                    , Procedure.jump global <|
                        \_ ->
                            pageUsersUserFormProcedures toCmd toEvent unwrapEvent pageUsers userForm
                    ]

                Just PageUsersClickRegisterNewUser ->
                    [ Procedure.push userForm <|
                        \_ _ -> toCmd PageUsersRequestRegisterNewUser
                    , Procedure.await userForm <|
                        \event2 _ ->
                            case unwrapEvent event2 of
                                Just (PageUsersReceiveRegisterNewUserResp (Err err)) ->
                                    [ handlePageUsersHttpError toCmd pageUsers RouteUsers err
                                    , Procedure.jump global <|
                                        \_ ->
                                            pageUsersUserFormProcedures toCmd toEvent unwrapEvent pageUsers userForm
                                    ]

                                Just (PageUsersReceiveRegisterNewUserResp (Ok user)) ->
                                    [ Procedure.modify userForm <|
                                        \memory ->
                                            { memory | newUserName = "" }
                                    , Procedure.async
                                        [ Procedure.insertAfter users userForm (initUserForm user) <|
                                            pageUsersUserFormProcedures toCmd toEvent unwrapEvent pageUsers
                                        ]
                                    ]

                                _ ->
                                    []
                    ]

                _ ->
                    []
    ]


handlePageUsersHttpError : (PageUsersCmd -> cmd) -> Observer memory PageUsersMemory -> Route -> HttpError -> Procedure cmd memory event
handlePageUsersHttpError toCmd pageUsers route err =
    Procedure.batch <|
        case err of
            LoginRequired ->
                [ Procedure.push pageUsers <|
                    \_ _ ->
                        toCmd <|
                            PageUsersPushRoute <|
                                RouteLogin { back = route }
                ]

            SomeError str ->
                [ putPageUsersLog toCmd pageUsers str
                ]


putPageUsersLog : (PageUsersCmd -> cmd) -> Observer memory PageUsersMemory -> String -> Procedure cmd memory event
putPageUsersLog toCmd pageUsers str =
    Procedure.push pageUsers <|
        \_ _ -> toCmd <| PageUsersDisplayLog <| "[Page.Users] " ++ str
