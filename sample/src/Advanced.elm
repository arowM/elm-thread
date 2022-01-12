module Advanced exposing (Event, Form, GoatCard, Memory, Saved, main)

import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Thread.Browser as Browser exposing (Document, Program)
import Thread.Lifter as Lifter exposing (Lifter)
import Thread.LocalMemory as LocalMemory exposing (LocalMemory)
import Thread.Procedure as Procedure exposing (Block, Msg)
import Thread.ThreadId as ThreadId exposing (ThreadId)


main : Program () Memory Event
main =
    Browser.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = Browser.globalSubscriptions subscriptions
        }



-- Memory


{-| The memory state shared by all threads.
-}
type alias Memory =
    { cards : LocalMemory GoatCard
    }


init : Memory
init =
    { cards = LocalMemory.init
    }



-- -- Goat Card


{-| Local memory for a goat card, which is used to manage a personal information for a goat.
-}
type alias GoatCard =
    { form : Form
    , saved : Saved
    , onEditing : Bool
    }


type alias Form =
    { name : String
    }


type alias Saved =
    { name : String
    }


initGoatCard : GoatCard
initGoatCard =
    { form =
        { name = ""
        }
    , saved =
        { name = ""
        }
    , onEditing = True
    }



-- Event


{-| Events that only affect a specific thread.
-}
type Event
    = ClickAddGoatCard
      -- For each goat card
    | ClickEditGoat
    | ClickRemoveGoat
    | ClickSaveGoat
    | ClickCancelGoat
    | ChangeGoatName String



-- View


view : ThreadId -> Memory -> Document (Msg Event)
view tid shared =
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
                    , Events.onClick
                        (ClickAddGoatCard
                            |> Procedure.setTarget tid
                        )
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div"
                [ style "padding" "0.4em"
                ]
                (LocalMemory.toList shared.cards
                    |> List.reverse
                    |> List.map
                        (\( rid, goatCard ) ->
                            ( ThreadId.toString rid
                            , goatCardView rid goatCard
                            )
                        )
                )
            ]
        ]
    }



-- -- Goat card


goatCardView : ThreadId -> GoatCard -> Html (Msg Event)
goatCardView tid memory =
    if memory.onEditing then
        editModeGoatCardView tid memory.form

    else
        savedModeGoatCardView tid memory.saved


{-| -}
editModeGoatCardView : ThreadId -> Form -> Html (Msg Event)
editModeGoatCardView tid form =
    let
        toMsg =
            Procedure.setTarget tid
    in
    Html.div
        [ style "border" "solid #333 2px"
        , style "border-radius" "0.2em"
        ]
        [ Html.div
            [ style "display" "inline-block"
            ]
            [ Html.span
                [ style "padding" "0.4em"
                ]
                [ Html.text "name:"
                ]
            , Html.input
                [ style "margin" "0.4em"
                , onChange (toMsg << ChangeGoatName)
                , Attributes.value form.name
                ]
                []
            ]
        , Html.div
            [ style "display" "inline-block"
            ]
            [ Html.div
                [ style "padding" "0.4em"
                , style "display" "inline-block"
                ]
                [ Html.button
                    [ style "padding" "0.4em"
                    , Attributes.type_ "button"
                    , Events.onClick (toMsg ClickCancelGoat)
                    ]
                    [ Html.text "Cancel"
                    ]
                , Html.button
                    [ style "padding" "0.4em"
                    , Attributes.type_ "button"
                    , Events.onClick (toMsg ClickSaveGoat)
                    ]
                    [ Html.text "Save"
                    ]
                ]
            ]
        ]


savedModeGoatCardView : ThreadId -> Saved -> Html (Msg Event)
savedModeGoatCardView tid saved =
    let
        toMsg =
            Procedure.setTarget tid
    in
    Html.div
        [ style "border" "solid #333 2px"
        , style "border-radius" "0.2em"
        , style "padding" "0.4em"
        , style "min-width" "16em"
        ]
        [ Html.div
            []
            [ Html.div
                [ style "text-align" "right"
                , style "color" "#733"
                ]
                [ Html.span
                    [ style "padding" "0.4em"
                    , style "text-decoration" "underline"
                    , style "cursor" "pointer"
                    , Attributes.tabindex 0
                    , Attributes.attribute "role" "button"
                    , Events.onClick (toMsg ClickEditGoat)
                    ]
                    [ Html.text "edit"
                    ]
                , Html.span
                    [ style "padding" "0.4em"
                    , style "text-decoration" "underline"
                    , style "cursor" "pointer"
                    , Attributes.tabindex 0
                    , Attributes.attribute "role" "button"
                    , Events.onClick (toMsg ClickRemoveGoat)
                    ]
                    [ Html.text "remove"
                    ]
                ]
            , Html.div
                [ style "padding" "0.4em"
                ]
                [ Html.text <| "name: " ++ saved.name
                ]
            ]
        ]



-- Subsctiption


subscriptions : Memory -> Sub Event
subscriptions _ =
    Sub.none



-- Procedure


procedures : () -> Block Memory Event
procedures () _ =
    [ Procedure.await <|
        \event _ ->
            case event of
                ClickAddGoatCard ->
                    [ LocalMemory.async
                        { get = .cards >> Just
                        , set = \cards shared -> { shared | cards = cards }
                        }
                        initGoatCard
                        goatCardProcedures
                    ]

                _ ->
                    []
    , Procedure.jump <| procedures ()
    ]



-- -- Goat card


{-| This procedure is completed only when the remove button clicked.
-}
goatCardProcedures : Lifter Memory GoatCard -> Block Memory Event
goatCardProcedures =
    editModeGoatCardProcedures
        { isInitial = True
        }


{-| Procedure for editing mode.
-}
editModeGoatCardProcedures : { isInitial : Bool } -> Lifter Memory GoatCard -> Block Memory Event
editModeGoatCardProcedures opt l _ =
    [ Lifter.await l <|
        \event _ ->
            case event of
                ClickSaveGoat ->
                    [ Lifter.modify l <|
                        \card ->
                            { card
                                | form =
                                    { name = ""
                                    }
                                , saved =
                                    { name = card.form.name
                                    }
                                , onEditing = False
                            }
                    , Lifter.jump l savedModeGoatCardProcedures
                    ]

                ClickCancelGoat ->
                    [ Lifter.when l
                        (\_ -> opt.isInitial)
                        [ Procedure.quit
                        ]
                    , Lifter.modify l <|
                        \card ->
                            { card
                                | form =
                                    { name = ""
                                    }
                                , onEditing = False
                            }
                    , Lifter.jump l savedModeGoatCardProcedures
                    ]

                ChangeGoatName str ->
                    [ Lifter.modify l <| modifyForm <| \form -> { form | name = str }
                    , Lifter.jump l <| editModeGoatCardProcedures opt
                    ]

                _ ->
                    []
    ]


{-| Procedure for view mode.
-}
savedModeGoatCardProcedures : Lifter Memory GoatCard -> Block Memory Event
savedModeGoatCardProcedures l _ =
    [ Lifter.await l <|
        \event _ ->
            case event of
                ClickEditGoat ->
                    [ Lifter.modify l <|
                        \card ->
                            { card
                                | onEditing = True
                                , form =
                                    { name = card.saved.name
                                    }
                            }
                    , Lifter.jump l <|
                        editModeGoatCardProcedures
                            { isInitial = False
                            }
                    ]

                ClickRemoveGoat ->
                    [ Procedure.quit
                    ]

                _ ->
                    []
    ]



-- -- Helper functions


modifyForm : (Form -> Form) -> GoatCard -> GoatCard
modifyForm f card =
    { card | form = f card.form }
