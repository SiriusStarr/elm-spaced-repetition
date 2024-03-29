module SpacedRepetition.Internal.SMTwo exposing
    ( ReviewHistory(..), EFactor, Streak(..)
    , defaultEFactor
    , eFactor, eFactorToFloat, streakToInterval
    , decodeEFactor, encodeEFactor, decodeStreak, encodeStreak
    )

{-| Internal module for SMTwo algorithm.


## Opaque Types

@docs ReviewHistory, EFactor, Streak


## Values

@docs defaultEFactor


## Conversion Functions

@docs eFactor, eFactorToFloat, streakToInterval


## JSON Decoders/Encoders

@docs decodeEFactor, encodeEFactor, decodeStreak, encodeStreak

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import Time


{-| The current review history for a card:

  - `New` -- Never before reviewed.
  - `Reviewed` -- Reviewed correctly in the past.
  - `Repeating` -- Scheduled to be immediately reviewed again due to an incorrect response.

-}
type ReviewHistory
    = New
    | Reviewed { ease : EFactor, lastReviewed : Time.Posix, streak : Streak }
    | Repeating { ease : EFactor, streak : Streak }


{-| Opaque type for "ease". Must be greater than 1.3, default value of 2.5
, with larger values being "easier" (more time between reviews).
-}
type EFactor
    = EFactor Float


{-| The current streak of correct answers for a card. Cards with 2 or more
correct repetitions are not treated differently and are thus combined.
-}
type Streak
    = Zero
    | One
    | TwoPlus { interval : Natural }


{-| Default EFactor value for new cards.
-}
defaultEFactor : EFactor
defaultEFactor =
    EFactor 2.5


{-| Create an `EFactor` by ensuring a float is at least `1.3`.
-}
eFactor : Float -> EFactor
eFactor f =
    EFactor <| max 1.3 f


{-| Unwrap the opaque type `EFactor`.
-}
eFactorToFloat : EFactor -> Float
eFactorToFloat (EFactor f) =
    f


{-| Given how many times a card has been correctly answered in a row, determine the interval between reviews.
-}
streakToInterval : Streak -> Natural
streakToInterval streak =
    (case streak of
        Zero ->
            Natural.fromInt 1

        One ->
            Natural.fromInt 6

        TwoPlus { interval } ->
            Just interval
    )
        |> Maybe.withDefault Natural.nil


{-| Decode an`EFactor` as from JSON.
-}
decodeEFactor : Decoder EFactor
decodeEFactor =
    Decode.map eFactor Decode.float


{-| Encode an`EFactor` as JSON.
-}
encodeEFactor : EFactor -> Encode.Value
encodeEFactor eF =
    Encode.float <| eFactorToFloat eF


{-| Decode a `Streak` from JSON.
-}
decodeStreak : Decoder Streak
decodeStreak =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\string ->
                    case string of
                        "O" ->
                            Decode.succeed One

                        "Z" ->
                            Decode.succeed Zero

                        _ ->
                            Decode.fail "Invalid Streak"
                )
        , Decode.map ((\interval -> TwoPlus { interval = interval }) << Natural.max Natural.six) Natural.decode
        ]


{-| Encode a `Streak` as JSON.
-}
encodeStreak : Streak -> Encode.Value
encodeStreak streak =
    case streak of
        Zero ->
            Encode.string "Z"

        One ->
            Encode.string "O"

        TwoPlus { interval } ->
            Natural.encode interval
