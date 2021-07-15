module SpacedRepetition.Internal.SMTwo exposing
    ( EFactor
    , ReviewHistory(..)
    , Streak(..)
    , defaultEFactor
    , eFactor
    , eFactorToFloat
    , streakToInterval
    )

import Time


{-| Descriptive alias for the interval between reviews in days.
-}
type alias Interval =
    Int


{-| Descriptive alias for the last time a card was reviewed.
-}
type alias LastReviewed =
    Time.Posix


{-| The current streak of correct answers for a card. Cards with 2 or more
correct repetitions are not treated differently and are thus combined.
-}
type Streak
    = One
    | TwoPlus Interval
    | Zero


{-| The current review history for a card:

  - `New` -- Never before reviewed.
  - `Repeating` -- Scheduled to be immediately reviewed again due to an incorrect response.
  - `Reviewed` -- Reviewed correctly in the past.

-}
type ReviewHistory
    = New
    | Repeating EFactor Streak
    | Reviewed EFactor LastReviewed Streak


{-| Given how many times a card has been correctly answered in a row, determine the interval between reviews. -
-}
streakToInterval : Streak -> Int
streakToInterval streak =
    case streak of
        One ->
            6

        TwoPlus i ->
            i

        Zero ->
            1


{-| Opaque type for "ease". Must be greater than 1.3, default value of 2.5
, with larger values being "easier" (more time between reviews).
-}
type EFactor
    = EFactor Float


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
