module SpacedRepetition.Internal.SMTwoPlus exposing
    ( Difficulty
    , Interval
    , PerformanceRating
    , ReviewHistory(..)
    , createDifficulty
    , createInterval
    , decodeDifficulty
    , decodeInterval
    , defaultDifficulty
    , difficultyToFloat
    , encodeDifficulty
    , encodeInterval
    , intervalToFloat
    , performanceRating
    , performanceRatingToFloat
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Round exposing (roundNum)
import Time


{-| Opaque type for the difficulty of a card.
-}
type Difficulty
    = Difficulty Float


{-| The interval between scheduled reviews, in days.
-}
type Interval
    = Interval Float


{-| Opaque type for the "performance rating" (answer quality) for a card.
-}
type PerformanceRating
    = PerformanceRating Float


{-| The review history of a card:

  - `New` -- The card has not been reviewed yet.
  - `Reviewed` -- The card has been reviewed.

-}
type ReviewHistory
    = New
    | Reviewed
        { difficulty : Difficulty
        , interval : Interval
        , lastReviewed : Time.Posix
        }


{-| Turn a `Float` into a `Difficulty`, clamping it between 0 and 1.
-}
createDifficulty : Float -> Difficulty
createDifficulty f =
    Difficulty <| clamp 0.0 1.0 f


{-| Turn a `Float` into an `Interval`, rounding it to 4 decimal places and
ensuring it is at least 1.
-}
createInterval : Float -> Interval
createInterval f =
    roundNum 4 f
        |> max 1
        |> Interval


{-| Decode a difficulty from JSON.
-}
decodeDifficulty : Decoder Difficulty
decodeDifficulty =
    Decode.map createDifficulty Decode.float


{-| Decode an interval from JSON.
-}
decodeInterval : Decoder Interval
decodeInterval =
    Decode.map createInterval Decode.float


{-| The starting difficulty for new cards.
-}
defaultDifficulty : Difficulty
defaultDifficulty =
    Difficulty 0.3


{-| Unpack the opaque type `Difficulty`.
-}
difficultyToFloat : Difficulty -> Float
difficultyToFloat (Difficulty f) =
    f


{-| Encode a difficulty as JSON.
-}
encodeDifficulty : Difficulty -> Encode.Value
encodeDifficulty =
    Encode.float << difficultyToFloat


{-| Encode an interval as JSON.
-}
encodeInterval : Interval -> Encode.Value
encodeInterval =
    Encode.float << intervalToFloat


{-| Unpack the opaque type `Interval`.
-}
intervalToFloat : Interval -> Float
intervalToFloat (Interval f) =
    f


{-| The `performanceRating` function creates a `PerformanceRating`. `PerformanceRating` is quantitative and must be between 0.0 and 1.0, with values of 0.6 and greater representing a "correct" answer.
-}
performanceRating : Float -> PerformanceRating
performanceRating f =
    PerformanceRating <| clamp 0.0 1.0 f


{-| Unpack a`PerformanceRating` to a `Float`.
-}
performanceRatingToFloat : PerformanceRating -> Float
performanceRatingToFloat (PerformanceRating f) =
    f
