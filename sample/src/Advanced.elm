module Advanced exposing (Global, Local, Shared, main)

import Advanced.GoatCard as GoatCard
import Advanced.Resource as Resource exposing (Resource)
import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Keyed as Keyed
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Procedure as Procedure exposing (Procedure)


main : Program () Shared Global Local
main =
    Browser.document
        { init = init
        , procedure = procedure
        , view = view
        , subscriptions = subscriptions
        }


{-| The memory state shared by all threads.
-}
type alias Shared =
    { cards : Resource GoatCard.Shared
    }


init : Shared
init =
    { cards = Resource.empty
    }


{-| Global events
-}
type Global
    = GoatCardGlobal Resource.Id GoatCard.Global
    | ClickAddGoatCard


{-| Local events that only affect a specific thread.
-}
type Local
    = GoatCardLocal Resource.Id GoatCard.Local



-- View


view : Shared -> Document Global
view shared =
    { title = "Advanced sample app"
    , body =
        [ Html.div
            [ style "display" "inline-block"
            , style "padding" "0.4em"
            , style "margin" "0"
            ]
            [ Html.div
                [ style "padding" "0.4em"
                ]
                [ Html.button
                    [ Attributes.type_ "button"
                    , Events.onClick ClickAddGoatCard
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div"
                [ style "padding" "0.4em"
                ]
                (List.map
                    (\( rid, goatCard ) ->
                        ( Resource.toString rid
                        , GoatCard.view goatCard
                            |> Html.map (GoatCardGlobal rid)
                        )
                    )
                    (Resource.toList shared.cards)
                )
            ]
        ]
    }



-- Subsctiption


subscriptions : Shared -> Sub Global
subscriptions _ =
    Sub.none



-- Procedure


procedure : () -> Procedure Shared Global Local
procedure () =
    Procedure.batch
        [ Procedure.awaitGlobal <|
            \global _ ->
                case global of
                    ClickAddGoatCard ->
                        Just <|
                            Procedure.batch
                                [ Procedure.modifyAndThen addGoatCardAnd <|
                                    \rid ->
                                        Procedure.batch
                                            [ Procedure.fork <|
                                                \_ ->
                                                    Procedure.batch
                                                        [ GoatCard.procedure
                                                            |> fromGoatCardProcedure rid

                                                        -- Whenever a GoatCard Procedure is completed,
                                                        -- resources are released.
                                                        , Procedure.modify <|
                                                            \shared ->
                                                                { shared | cards = Resource.remove rid shared.cards }
                                                        ]
                                            ]
                                ]

                    _ ->
                        Nothing
        , Procedure.lazy <| \_ -> procedure ()
        ]


addGoatCardAnd : Shared -> ( Shared, Resource.Id )
addGoatCardAnd shared =
    let
        ( rid, newCards ) =
            Resource.push GoatCard.init shared.cards
    in
    ( { shared
        | cards = newCards
      }
    , rid
    )


fromGoatCardProcedure : Resource.Id -> Procedure GoatCard.Shared GoatCard.Global GoatCard.Local -> Procedure Shared Global Local
fromGoatCardProcedure target =
    Procedure.liftShared
        { get =
            \shared ->
                Resource.lookup target shared.cards
                    |> Maybe.withDefault GoatCard.init
        , set =
            \new shared ->
                { shared
                    | cards =
                        Resource.modify
                            (\id a ->
                                if id == target then
                                    new

                                else
                                    a
                            )
                            shared.cards
                }
        }
        >> Procedure.wrapLocal
            { unwrap =
                \local ->
                    case local of
                        GoatCardLocal id event ->
                            if id == target then
                                Just event

                            else
                                Nothing
            , wrap = GoatCardLocal target
            }
        >> Procedure.wrapGlobal
            (\global ->
                case global of
                    GoatCardGlobal id event ->
                        if id == target then
                            Just event

                        else
                            Nothing

                    _ ->
                        Nothing
            )
