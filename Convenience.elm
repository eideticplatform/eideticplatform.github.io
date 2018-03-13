module Convenience exposing (..)


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


{-| Take a function that returns `Maybe` value and a list. Map a function over each element of the list. Collect the result in the list within `Maybe`.
    traverse (\x -> Just (x*10)) [1,2,3,4,5] == Just [10,20,30,40,50]
-}
traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
        List.foldr step (Just [])
