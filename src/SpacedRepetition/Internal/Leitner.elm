module SpacedRepetition.Internal.Leitner exposing (Box(..), NumberOfBoxes(..))

import Time


{-| Descriptive alias for the date the card was last reviewed.
-}
type alias LastReviewed =
    Time.Posix


{-| Descriptive alias for which box a card is in.
-}
type alias BoxNumber =
    Int


{-| The current box that a card is in:

  - `New` -- Never studied.
  - `BoxN number lastReviewed` -- Card is in box `number` and was last studied `lastReviewed`.
  - `Graduated` -- Card has exceeded the maximum number of boxes and is finished.

-}
type Box
    = BoxN BoxNumber LastReviewed
    | Graduated
    | New


{-| The maximum number of boxes in the Leitner system, beyond which cards will
be graduated.
-}
type NumberOfBoxes
    = NumberOfBoxes Int
