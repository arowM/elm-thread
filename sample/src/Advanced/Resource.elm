module Advanced.Resource exposing
    ( Resource
    , Id
    , toString
    , empty
    , toList
    , lookup
    , push
    , modify
    , remove
    )

{-| Domain specific library for Advanced app.

@docs Resource
@docs Id
@docs toString
@docs empty
@docs toList
@docs lookup
@docs push
@docs modify
@docs remove

-}


{-| -}
type Resource a
    = Resource (Resource_ a)


type alias Resource_ a =
    { nextId : Id
    , list : List ( Id, a )
    }


{-| -}
empty : Resource a
empty =
    Resource
        { nextId = initId
        , list = []
        }


{-| -}
type Id
    = Id Int


initId : Id
initId =
    Id 0


incId : Id -> Id
incId (Id n) =
    Id (n + 1)


{-| -}
toString : Id -> String
toString (Id n) =
    String.fromInt n


{-| -}
lookup : Id -> Resource a -> Maybe a
lookup target (Resource resource) =
    List.filterMap
        (\( id, a ) ->
            if id == target then
                Just a

            else
                Nothing
        )
        resource.list
        |> List.head


{-| Push new resource. It returns pair of the ID for the created resource and the updated `Resource`.
-}
push : a -> Resource a -> ( Id, Resource a )
push a (Resource resource) =
    ( resource.nextId
    , Resource
        { resource
            | nextId = incId resource.nextId
            , list = resource.list ++ [ ( resource.nextId, a ) ]
        }
    )


{-| -}
toList : Resource a -> List ( Id, a )
toList (Resource resource) =
    resource.list


{-| -}
modify : (Id -> a -> a) -> Resource a -> Resource a
modify f (Resource resource) =
    Resource
        { resource
            | list =
                List.map (\( id, a ) -> ( id, f id a )) resource.list
        }


{-| -}
remove : Id -> Resource a -> Resource a
remove target (Resource resource) =
    Resource
        { resource
            | list = List.filter (\( id, _ ) -> id /= target) resource.list
        }
