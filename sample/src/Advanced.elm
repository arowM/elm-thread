module Advanced exposing (Event, Form, GoatCard, Memory, Saved, main)

import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Events.Extra exposing (onChange)
import Html.Keyed as Keyed
import Procedure exposing (Document, Msg, Observer, Program, global)
import Procedure.ObserverId as ObserverId exposing (ObserverId)


main : Program () Memory Event
main =
    Procedure.document
        { init = init
        , procedures = procedures
        , view = view
        , subscriptions = subscriptions
        }



-- Memory


{-| The memory state shared by all threads.
-}
type alias Memory =
    { cards : List ( ObserverId, GoatCard )
    }


init : Memory
init =
    { cards = []
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


view : Memory -> Document (Msg Event)
view memory =
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
                            |> Procedure.publish
                        )
                    ]
                    [ Html.text "Add new card"
                    ]
                ]
            , Keyed.node "div"
                [ style "padding" "0.4em"
                ]
                (memory.cards
                    |> List.map
                        (\( oid, goatCard ) ->
                            ( ObserverId.toString oid
                            , goatCardView oid goatCard
                            )
                        )
                )
            ]
        ]
    }



-- -- Goat card


goatCardView : ObserverId -> GoatCard -> Html (Msg Event)
goatCardView oid memory =
    if memory.onEditing then
        editModeGoatCardView oid memory.form

    else
        savedModeGoatCardView oid memory.saved


{-| -}
editModeGoatCardView : ObserverId -> Form -> Html (Msg Event)
editModeGoatCardView oid form =
    let
        toMsg =
            Procedure.issue oid
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


savedModeGoatCardView : ObserverId -> Saved -> Html (Msg Event)
savedModeGoatCardView oid saved =
    let
        toMsg =
            Procedure.issue oid
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


subscriptions : Memory -> Sub (Msg Event)
subscriptions _ =
    Sub.none



-- Procedure


type alias Procedure =
    Procedure.Procedure (Cmd (Msg Event)) Memory Event


procedures : () -> List Procedure
procedures () =
    [ Procedure.await global <|
        \event _ ->
            case event of
                ClickAddGoatCard ->
                    [ Procedure.async
                        [ Procedure.append
                            (global
                                |> Procedure.dig
                                    { get = .cards
                                    , set = \c memory -> { memory | cards = c }
                                    }
                            )
                            initGoatCard
                            goatCardProcedures
                        ]
                    ]

                _ ->
                    []
    , Procedure.jump global <| \_ -> procedures ()
    ]



-- -- Goat card


{-| This procedure is completed only when the remove button clicked.
-}
goatCardProcedures : Observer Memory GoatCard -> List Procedure
goatCardProcedures card =
    editModeGoatCardProcedures card
        { isInitial = True
        }


{-| Procedure for editing mode.
-}
editModeGoatCardProcedures : Observer Memory GoatCard -> { isInitial : Bool } -> List Procedure
editModeGoatCardProcedures card opt =
    let
        form : Observer Memory Form
        form =
            card
                |> Procedure.dig
                    { get = .form
                    , set = \a memory -> { memory | form = a }
                    }
    in
    [ Procedure.await card <|
        \event _ ->
            case event of
                ClickSaveGoat ->
                    [ Procedure.modify card <|
                        \memory ->
                            { memory
                                | form =
                                    { name = ""
                                    }
                                , saved =
                                    { name = memory.form.name
                                    }
                                , onEditing = False
                            }
                    , Procedure.jump global <| \_ -> savedModeGoatCardProcedures card
                    ]

                ClickCancelGoat ->
                    [ Procedure.when opt.isInitial
                        [ Procedure.quit
                        ]
                    , Procedure.modify card <|
                        \memory ->
                            { memory
                                | form =
                                    { name = ""
                                    }
                                , onEditing = False
                            }
                    , Procedure.jump global <| \_ -> savedModeGoatCardProcedures card
                    ]

                ChangeGoatName str ->
                    [ Procedure.modify form <|
                        \memory -> { memory | name = str }
                    , Procedure.jump global <| \_ -> editModeGoatCardProcedures card opt
                    ]

                _ ->
                    []
    ]


{-| Procedure for view mode.
-}
savedModeGoatCardProcedures : Observer Memory GoatCard -> List Procedure
savedModeGoatCardProcedures card =
    [ Procedure.await card <|
        \event _ ->
            case event of
                ClickEditGoat ->
                    [ Procedure.modify card <|
                        \memory ->
                            { memory
                                | onEditing = True
                                , form =
                                    { name = memory.saved.name
                                    }
                            }
                    , Procedure.jump global <|
                        \_ ->
                            editModeGoatCardProcedures
                                card
                                { isInitial = False
                                }
                    ]

                ClickRemoveGoat ->
                    [ Procedure.quit
                    ]

                _ ->
                    []
    ]
