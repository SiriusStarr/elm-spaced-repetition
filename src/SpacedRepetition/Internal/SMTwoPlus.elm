module SpacedRepetition.Internal.SMTwoPlus exposing
    ( Difficulty
    , Interval
    , PerformanceRating
    , ReviewHistory(..)
    , createDifficulty
    , createInterval
    , difficultyToFloat
    , intervalToFloat
    , performanceRating
    , performanceRatingToFloat
    )

import Round exposing (roundNum)
import Time


{-| Opaque type for the difficulty of a card.
-}
type Difficulty
    = Difficulty Float


{-| Descriptive alias for the date the card was last reviewed.
-}
type alias LastReviewed =
    Time.Posix


{-| The interval between scheduled reviews, in days.
-}
type Interval
    = Interval Float


{-| The review history of a card:

  - `New` -- The card has not been reviewed yet.
  - `Reviewed` -- The card has been reviewed.

-}
type ReviewHistory
    = New
    | Reviewed Difficulty LastReviewed Interval


{-| Opaque type for the "performance rating" (answer quality) for a card.
-}
type PerformanceRating
    = PerformanceRating Float


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


{-| Unpack the opaque type `Difficulty`.
-}
difficultyToFloat : Difficulty -> Float
difficultyToFloat (Difficulty f) =
    f


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


{-| Unpack the opaque type `Interval`.
-}
intervalToFloat : Interval -> Float
intervalToFloat (Interval f) =
    f
