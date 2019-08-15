module SpacedRepetition.Internal.Leitner exposing (Box(..), NumberOfBoxes(..))

import Time


type alias DateLastReviewed =
    Time.Posix


type alias BoxNumber =
    Int


type Box
    = New
    | BoxN BoxNumber DateLastReviewed
    | Graduated


type NumberOfBoxes
    = NumberOfBoxes Int
