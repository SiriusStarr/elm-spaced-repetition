module SpacedRepetition.Internal.Natural exposing (Natural, compare, decode, encode, fromInt, nil, pred, succ, toInt)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| A natural number.
-}
type Natural
    = Natural Int


{-| Zero.
-}
nil : Natural
nil =
    Natural 0


{-| Get the number after the current one.
-}
succ : Natural -> Natural
succ (Natural i) =
    Natural <| i + 1


{-| Get the number before the current one, or stay at zero.
-}
pred : Natural -> Natural
pred (Natural i) =
    Natural <| max 0 <| i - 1


{-| Convert an `Int` to a `Natural` or fail.
-}
fromInt : Int -> Maybe Natural
fromInt i =
    if i < 0 then
        Nothing

    else
        Just <| Natural i


{-| Unpack a `Natural` into an `Int`.
-}
toInt : Natural -> Int
toInt (Natural i) =
    i


{-| Compare to naturals and return an ordering.
-}
compare : Natural -> Natural -> Order
compare (Natural i) (Natural j) =
    if i < j then
        LT

    else if i > j then
        GT

    else
        EQ


{-| JSON encoder for `Natural`.
-}
encode : Natural -> Encode.Value
encode (Natural i) =
    Encode.int i


{-| JSON decoder for `Natural`.
-}
decode : Decoder Natural
decode =
    Decode.int
        |> Decode.andThen
            (\i ->
                case fromInt i of
                    Just n ->
                        Decode.succeed n

                    Nothing ->
                        Decode.fail <| "Invalid natural: " ++ String.fromInt i
            )
