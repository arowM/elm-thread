module Advanced.GoatCard exposing
    ( Memory
    , Event
    , init
    , procedures
    , view
    )

{-| Editable profile card about a goat.

@docs Memory
@docs Event
@docs init
@docs procedures
@docs view

-}

import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Events.Extra exposing (onChange)
import Thread.Procedure as Procedure exposing (Block, Procedure)


{-| -}
type Memory
    = Memory Memory_


type alias Memory_ =
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


{-| -}
init : Memory
init =
    Memory
        { form =
            { name = ""
            }
        , saved =
            { name = ""
            }
        , onEditing = True
        }


{-| -}
type Event
    = ClickEdit
    | ClickRemove
    | ClickSave
    | ClickCancel
    | ChangeName String



-- Procedure


{-| This procedure is completed only when the remove button clicked.
-}
procedures : Block Memory Event
procedures =
    editModeProcedures
        { isInitial = True
        }


{-| Procedure for editing mode.
-}
editModeProcedures : { isInitial : Bool } -> Block Memory Event
editModeProcedures opt _ =
    [ Procedure.await <|
        \event _ ->
            case event of
                ClickSave ->
                    [ Procedure.modify <|
                        \(Memory memory) ->
                            Memory
                                { memory
                                    | form =
                                        { name = ""
                                        }
                                    , saved =
                                        { name = memory.form.name
                                        }
                                    , onEditing = False
                                }
                    , Procedure.jump savedModeProcedures
                    ]

                ClickCancel ->
                    [ Procedure.when (\_ -> opt.isInitial)
                        [ Procedure.quit
                        ]
                    , Procedure.modify <|
                        \(Memory memory) ->
                            Memory
                                { memory
                                    | form =
                                        { name = ""
                                        }
                                    , onEditing = False
                                }
                    , Procedure.jump savedModeProcedures
                    ]

                ChangeName str ->
                    [ modifyForm <| \form -> { form | name = str }
                    , Procedure.jump <| editModeProcedures opt
                    ]

                _ ->
                    []
    ]


{-| Procedure for view mode.
-}
savedModeProcedures : Block Memory Event
savedModeProcedures _ =
    [ Procedure.await <|
        \event _ ->
            case event of
                ClickEdit ->
                    [ Procedure.modify <|
                        \(Memory memory) ->
                            Memory
                                { memory
                                    | onEditing = True
                                    , form =
                                        { name = memory.saved.name
                                        }
                                }
                    , Procedure.jump <|
                        editModeProcedures
                            { isInitial = False
                            }
                    ]

                ClickRemove ->
                    [ Procedure.quit
                    ]

                _ ->
                    []
    ]



-- -- Helper procedures


modifyForm : (Form -> Form) -> Procedure Memory Event
modifyForm f =
    Procedure.modify <|
        \(Memory memory) ->
            Memory { memory | form = f memory.form }



-- View


{-| -}
view : Memory -> Html Event
view (Memory memory) =
    if memory.onEditing then
        editModeView memory.form

    else
        savedModeView memory.saved


{-| -}
editModeView : Form -> Html Event
editModeView form =
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
                , onChange ChangeName
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
                    , Events.onClick ClickCancel
                    ]
                    [ Html.text "Cancel"
                    ]
                , Html.button
                    [ style "padding" "0.4em"
                    , Attributes.type_ "button"
                    , Events.onClick ClickSave
                    ]
                    [ Html.text "Save"
                    ]
                ]
            ]
        ]


savedModeView : Saved -> Html Event
savedModeView saved =
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
                    , Events.onClick ClickEdit
                    ]
                    [ Html.text "edit"
                    ]
                , Html.span
                    [ style "padding" "0.4em"
                    , style "text-decoration" "underline"
                    , style "cursor" "pointer"
                    , Attributes.tabindex 0
                    , Attributes.attribute "role" "button"
                    , Events.onClick ClickRemove
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
