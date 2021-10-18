module Advanced.GoatCard exposing
    ( Shared
    , Global
    , Local
    , init
    , procedure
    , view
    )

{-| Editable profile card about a goat.

@docs Shared
@docs Global
@docs Local
@docs init
@docs procedure
@docs view

-}

import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Html.Events.Extra exposing (onChange)
import Thread.Procedure as Procedure exposing (Procedure)


{-| -}
type Shared
    = Shared Shared_


type alias Shared_ =
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
init : Shared
init =
    Shared
        { form =
            { name = ""
            }
        , saved =
            { name = ""
            }
        , onEditing = True
        }


{-| -}
type Global
    = ClickEdit
    | ClickRemove
    | ClickSave
    | ClickCancel
    | ChangeName String


{-| -}
type alias Local =
    ()


{-| This procedure is completed only when the remove button clicked.
-}
procedure : Procedure Shared Global Local
procedure =
    editModeProcedure


{-| Procedure for editing mode.
-}
editModeProcedure : Procedure Shared Global Local
editModeProcedure =
    Procedure.batch
        [ Procedure.awaitGlobal <|
            \global _ ->
                case global of
                    ClickSave ->
                        Just <|
                            Procedure.batch
                                [ Procedure.modify <|
                                    \(Shared shared) ->
                                        Shared
                                            { shared
                                                | form =
                                                    { name = ""
                                                    }
                                                , saved =
                                                    { name = shared.form.name
                                                    }
                                                , onEditing = False
                                            }
                                , savedModeProcedure
                                ]

                    ClickCancel ->
                        Just <|
                            Procedure.batch
                                [ Procedure.modify <|
                                    \(Shared shared) ->
                                        Shared
                                            { shared
                                                | form =
                                                    { name = ""
                                                    }
                                                , onEditing = False
                                            }
                                , savedModeProcedure
                                ]

                    ChangeName str ->
                        Just <|
                            Procedure.batch
                                [ Procedure.modify <|
                                    \(Shared shared) ->
                                        Shared
                                            { shared
                                                | form =
                                                    shared.form
                                                        |> (\form ->
                                                                { form
                                                                    | name = str
                                                                }
                                                           )
                                            }
                                , Procedure.lazy (\_ -> editModeProcedure)
                                ]

                    _ ->
                        Nothing
        ]


{-| Procedure for view mode.
-}
savedModeProcedure : Procedure Shared Global Local
savedModeProcedure =
    Procedure.batch
        [ Procedure.awaitGlobal <|
            \global _ ->
                case global of
                    ClickEdit ->
                        Just <|
                            Procedure.batch
                                [ Procedure.modify <|
                                    \(Shared shared) ->
                                        Shared
                                            { shared
                                                | onEditing = True
                                                , form =
                                                    { name = shared.saved.name
                                                    }
                                            }
                                , editModeProcedure
                                ]

                    ClickRemove ->
                        Just <| Procedure.quit

                    _ ->
                        Nothing
        ]


{-| -}
view : Shared -> Html Global
view (Shared shared) =
    if shared.onEditing then
        editModeView shared.form

    else
        savedModeView shared.saved


{-| -}
editModeView : Form -> Html Global
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


savedModeView : Saved -> Html Global
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
