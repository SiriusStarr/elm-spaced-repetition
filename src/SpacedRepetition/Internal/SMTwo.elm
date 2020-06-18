module SpacedRepetition.Internal.SMTwo exposing
    ( EFactor
    , ReviewHistory(..)
    , Streak(..)
    , eFactor
    , eFactorToFloat
    , streakToInterval
    )

import Time


type alias Interval =
    Int


type alias PriorDate =
    Time.Posix


type Streak
    = Zero
    | One
    | TwoPlus Interval


type ReviewHistory
    = New
    | Reviewed EFactor PriorDate Streak
    | Repeating EFactor Streak


type EFactor
    = EFactor Float -- Greater than 1.3, default 2.5, larger "easier"


eFactor : Float -> EFactor
eFactor f =
    EFactor <| max 1.3 f


streakToInterval : Streak -> Int
streakToInterval streak =
    case streak of
        Zero ->
            1

        One ->
            6

        TwoPlus i ->
            i


eFactorToFloat : EFactor -> Float
eFactorToFloat e =
    case e of
        EFactor f ->
            f
