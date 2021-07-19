module SpacedRepetition.Internal.Leitner exposing (Box(..), NumberOfBoxes, highestBoxIndex, numberOfBoxes)

import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import Time


{-| The current box that a card is in:

  - `New` -- Never studied.
  - `BoxN {box, lastReviewed}` -- Card is in box `box` and was last studied `lastReviewed`.
  - `Graduated` -- Card has exceeded the maximum number of boxes and is finished.

-}
type Box
    = BoxN { box : Natural, lastReviewed : Time.Posix }
    | Graduated
    | New


{-| Get the index of the highest box in the system, given the `NumberOfBoxes`.
-}
highestBoxIndex : NumberOfBoxes -> Natural
highestBoxIndex (HighestBoxIndex n) =
    n


{-| The maximum number of boxes in the Leitner system, beyond which cards will
be graduated, as created by `numberOfBoxes`.
-}
type NumberOfBoxes
    = HighestBoxIndex Natural


{-| `numberOfBoxes` may be used to specify the total number of boxes before a
card "graduates" (i.e. is no longer reviewed). It takes an integer as a
parameter, specifying a system with that integer number of boxes. There must,
of course, be at least 1 box in the system (and there should almost certainly
be more).
-}
numberOfBoxes : Int -> NumberOfBoxes
numberOfBoxes =
    max 1
        >> Natural.fromInt
        >> Maybe.withDefault Natural.nil
        >> Natural.pred
        >> HighestBoxIndex
