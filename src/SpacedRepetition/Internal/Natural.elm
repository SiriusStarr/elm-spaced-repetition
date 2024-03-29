module SpacedRepetition.Internal.Natural exposing
    ( Natural
    , nil, six, eight
    , toInt, toFloat, fromInt
    , compare, max, pred, succ
    , decode, encode
    )

{-| Internal module for natural numbers.


## Type

@docs Natural


## Values

@docs nil, six, eight


## Conversion Functions

@docs toInt, toFloat, fromInt


## Manipulation Functions

@docs compare, max, pred, succ


## JSON Decoders/Encoders

@docs decode, encode

-}

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


{-| 6
-}
six : Natural
six =
    Natural 6


{-| 8
-}
eight : Natural
eight =
    Natural 8


{-| Unpack a `Natural` into an `Int`.
-}
toInt : Natural -> Int
toInt (Natural i) =
    i


{-| Cast a `Natural` to a `Float`.
-}
toFloat : Natural -> Float
toFloat (Natural i) =
    Basics.toFloat i


{-| Convert an `Int` to a `Natural` or fail.
-}
fromInt : Int -> Maybe Natural
fromInt i =
    if i < 0 then
        Nothing

    else
        Just <| Natural i


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


{-| Find the larger of two `Natural`s.
-}
max : Natural -> Natural -> Natural
max (Natural i) (Natural j) =
    Natural <| Basics.max i j


{-| Get the number before the current one, or stay at zero.
-}
pred : Natural -> Natural
pred (Natural i) =
    Natural <| Basics.max 0 <| i - 1


{-| Get the number after the current one.
-}
succ : Natural -> Natural
succ (Natural i) =
    Natural <| i + 1


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


{-| JSON encoder for `Natural`.
-}
encode : Natural -> Encode.Value
encode (Natural i) =
    Encode.int i
