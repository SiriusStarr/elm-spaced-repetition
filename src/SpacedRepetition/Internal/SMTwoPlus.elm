module SpacedRepetition.Internal.SMTwoPlus exposing
    ( Difficulty
    , Interval
    , PerformanceRating(..)
    , ReviewHistory(..)
    , createDifficulty
    , createInterval
    , difficultyToFloat
    , intervalToFloat
    , performanceRatingToFloat
    )

import Round exposing (roundNum)
import Time


type Difficulty
    = Difficulty Float


type alias DateLastReviewed =
    Time.Posix


type Interval
    = Interval Float


type ReviewHistory
    = New
    | Reviewed Difficulty DateLastReviewed Interval


type PerformanceRating
    = PerformanceRating Float


performanceRatingToFloat : PerformanceRating -> Float
performanceRatingToFloat perf =
    case perf of
        PerformanceRating f ->
            f


difficultyToFloat : Difficulty -> Float
difficultyToFloat diff =
    case diff of
        Difficulty f ->
            f


createDifficulty : Float -> Difficulty
createDifficulty f =
    Difficulty <| clamp 0.0 1.0 f


createInterval : Float -> Interval
createInterval f =
    roundNum 4 f
        |> max 1
        |> Interval


intervalToFloat : Interval -> Float
intervalToFloat interval =
    case interval of
        Interval f ->
            f
