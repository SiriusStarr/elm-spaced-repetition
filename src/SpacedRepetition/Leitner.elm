module SpacedRepetition.Leitner exposing
    ( LeitnerSettings, OnIncorrect(..), SpacingFunction, doubleSpacing, fibonacciSpacing, numberOfBoxes, NumberOfBoxes
    , Card, Deck
    , SRSData, newSRSData
    , encoderSRSData, decoderSRSData
    , Answer(..), answerCardInDeck, answerCard
    , getDueCardIndices, getDueCardIndicesWithDetails
    , QueueDetails(..), getCardDetails
    )

{-| This package provides everything necessary to create spaced repetition software using a variant of the Leitner system. The Leitner system was proposed by Sebastian Leitner in the early 1970s and was originally intended for use with physical (paper) flashcards.

For the basics about this algorithm, please refer to the [following description](https://en.wikipedia.org/wiki/Leitner_system) on Wikipedia.


# Settings

This algorithm requires certain settings be provided when functions are called to specify the behavior of the (very general) system.

@docs LeitnerSettings, OnIncorrect, SpacingFunction, doubleSpacing, fibonacciSpacing, numberOfBoxes, NumberOfBoxes


# Cards and Decks

The building blocks of this package are `Card`s and `Deck`s. In simple terms, a `Card` may be thought of as a single flashcard and a `Deck` as a list or collection of `Card`s.

@docs Card, Deck


# Card Data

A `Card` may be created by use of the `newSRSData` function, as in the following example:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

    myFlashcard : MyFlashcard
    myFlashcard =
        { prompt = "SYN"
        , answer = "SYN-ACK"
        , srsData = newSRSData
        }

@docs SRSData, newSRSData


# Json Encoders/Decoders

Since `Card` data must necessarily be preserved between sessions, a Json encoder/decoder is provided for `SRSData`. It may be utilized as follows:

    import Json.Decode as Decode
    import Json.Encode as Encode

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

    myFlashcardConstructor : SRSData -> String -> String -> MyFlashcard
    myFlashcardConstructor srsData prompt answer =
        { prompt = prompt
        , answer = answer
        , srsData = srsData
        }

    myFlashcardToJson : MyFlashcard -> String
    myFlashcardToJson myCard =
        Encode.encode 0 <|
            Encode.object
                [ ( "srsData", encoderSRSData myCard.srsData )
                , ( "prompt", Encode.string myCard.prompt )
                , ( "answer", Encode.string myCard.answer )
                ]

    myFlashcardDecoder : Decode.Decoder MyFlashcard
    myFlashcardDecoder =
        Decode.map3 myFlashcardConstructor
            (Decode.field "srsData" decoderSRSData)
            (Decode.field "prompt" Decode.string)
            (Decode.field "answer" Decode.string)

    jsonToMyFlashcard : String -> Result Decode.Error MyFlashcard
    jsonToMyFlashcard str =
        Decode.decodeString myFlashcardDecoder str

@docs encoderSRSData, decoderSRSData


# Answering Cards

The Leitner system depends only on answers being "correct" or "incorrect." Nevertheless, answers are additionally provided for "manually" moving cards, e.g. if you wish to implement a system in which the user decides how to move the card (this is usually a bad idea, since the point of an SRS system is to schedule cards better than people are able to estimate themselves).

@docs Answer, answerCardInDeck, answerCard


# Due Cards

Besides answering cards, this package handles determining which cards in a `Deck` are due and require study.

@docs getDueCardIndices, getDueCardIndicesWithDetails


# Card Details

If you require specific details for a single card, you may use the provided functionality here. If you need details for _all_ due cards, just use `getDueCardIndicesWithDetails`.

@docs QueueDetails, getCardDetails

-}

import Array exposing (Array)
import Array.Extra as ArrayX
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListX
import SpacedRepetition.Internal.Leitner exposing (Box(..), highestBoxIndex)
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import SpacedRepetition.Internal.Time as Time
import Time
import Time.Extra exposing (Interval(..), diff)


{-| `LeitnerSettings` customizes the behavior of this algorithm. Three parameters must be defined: the behavior of the system upon an incorrect answer, the spacing (interval) between "boxes", and the total number of boxes before a card "graduates." No builder functions are provided, as only three settings exist and the Leitner system doesn't have "defaults" to speak of. Additionally, no JSON encoder/decoder is provided because serializing functions (for `SpacingFunction`) is non-trivial.
-}
type alias LeitnerSettings =
    { onIncorrect : OnIncorrect
    , boxSpacing : SpacingFunction
    , numBoxes : NumberOfBoxes
    }


{-| `OnIncorrect` specifies what should happen to a card when an incorrect answer is given. In the "traditional" Leitner system, the card goes all the way back to the first box (`BackToStart`), however many variants simply move the card back one box (`BackOneBox`). This behavior may be overridden for a specific answer as discussed with the `Answer` type.
-}
type OnIncorrect
    = BackOneBox
    | BackToStart


{-| `SpacingFunction` takes an integer argument, representing the box number, and returns an integer representing the interval in days that cards in that box should go between reviews. **Note that box numbering starts at zero** (`0`), and intervals will always be at least 1 day (regardless of the output of a `SpacingFunction`). A custom function may be provided, or the pre-made functions `doubleSpacing` or `fibonacciSpacing` may be used. For obvious reasons, care should be taken that the complexity/recursive depth/etc. does not become excessive within the number of boxes your system will use.
-}
type alias SpacingFunction =
    Int -> Int


{-| `doubleSpacing` is a `SpacingFunction` with which the interval between each box simply doubles, e.g. Box 0 has an interval of `1`, Box 1 of `2`, Box 2 of `4`, Box 3 of `8`, etc.
-}
doubleSpacing : SpacingFunction
doubleSpacing boxNumber =
    -- Box Numbering starts at 0
    2 ^ boxNumber


{-| `fibonacciSpacing` is a `SpacingFunction` with which the interval between each box follows the Fibonacci sequence, e.g. Box 0 has an interval of `1`, Box 1 of `1`, Box 2 of `2`, Box 3 of `3`, Box 4 of `5`, Box 5 of `8`, Box 6 of `13`, etc.
-}
fibonacciSpacing : SpacingFunction
fibonacciSpacing box =
    let
        go : Int -> Int -> Int -> Int
        go n a b =
            case n of
                0 ->
                    a

                1 ->
                    b

                _ ->
                    go (n - 1) b (a + b)
    in
    go (1 + box) 0 1


{-| `numberOfBoxes` may be used to specify the total number of boxes before a card "graduates" (i.e. is no longer reviewed). It takes an integer as a parameter, specifying a system with that integer number of boxes. There must, of course, be at least 1 box in the system (and there should almost certainly be more), so values `< 1` will be ignored and result in a system with only one box.
-}
numberOfBoxes : Int -> NumberOfBoxes
numberOfBoxes =
    SpacedRepetition.Internal.Leitner.numberOfBoxes


{-| The maximum number of boxes in the Leitner system, beyond which cards will
be graduated, as created by `numberOfBoxes`.
-}
type alias NumberOfBoxes =
    SpacedRepetition.Internal.Leitner.NumberOfBoxes


{-| A `Card` represents a single question or unit of knowledge that the user will review. In general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields are required for a use case may simply be included in the record, e.g.:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

A `Card` by default contains only the information necessary for scheduling and nothing else; all other information should be added as in the above example.

-}
type alias Card a =
    { a | srsData : SRSData }


{-| A `Deck` represents a list of cards to be studied (this might be called a "collection" in other software). It is a record with field `cards` holding an `Array` of `Card` and field `settings` holding `LeitnerSettings`. Maintaining the state of a `Deck` may be handled by the user of the module or by this module itself. In general, it is probably best not to add a massive quantity of new (unstudied) cards to a deck at once.
-}
type alias Deck a b =
    { a
        | cards : Array (Card b)
        , settings : LeitnerSettings
    }


{-| `SRSData` contains all data necessary for the Leitner system and may be created with the `newSRSData` function. It may additionally be saved/loaded using the Json encoder/decoder in this package
-}
type alias SRSData =
    Box


{-| `newSRSData` creates a new `SRSData` for inclusion in a `Card`.
-}
newSRSData : SRSData
newSRSData =
    New


{-| `encoderSRSData` provides a Json encoder for encoding `SRSData` from a `Card`.
-}
encoderSRSData : SRSData -> Encode.Value
encoderSRSData data =
    case data of
        New ->
            Encode.null

        BoxN { box, lastReviewed } ->
            Encode.object
                [ ( "boxNum", Natural.encode box )
                , ( "reviewDate", Time.encode lastReviewed )
                ]

        Graduated ->
            Encode.string "G"


{-| `decoderSRSData` provides a Json decoder for decoding `SRSData` for a `Card`.
-}
decoderSRSData : Decode.Decoder SRSData
decoderSRSData =
    Decode.oneOf
        [ Decode.null New
        , Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "G" ->
                            Decode.succeed Graduated

                        _ ->
                            Decode.fail "Invalid SRS Data."
                )
        , Decode.map2 (\box lastReviewed -> BoxN { box = box, lastReviewed = lastReviewed })
            (Decode.field "boxNum" Natural.decode)
            (Decode.field "reviewDate" Time.decode)
        ]


{-| `Answer` must be passed to `answerCard`/`answerCardInDeck` when the user answers a card. It is usually best to simply use `Correct` and `Incorrect` (which follow the behavior specified in `LeitnerSettings`), but one could potentially provide the user with more options.

  - `Correct` -- The card was answered correctly, so advance it one step.
  - `Incorrect` -- The card was answered incorrectly; the box behavior depends
    on `OnIncorrect`.
  - `Pass` -- Leave the card in its current box; this will however graduate a
    card if it is in an invalid box larger than `NumberOfBoxes` (due e.g. to
    settings being changed).
  - `MoveBoxes i` -- Specify the number of boxes to move the card, with positive
    integers advancing the card (typically meaning longer intervals, depending
    on the `SpacingFunction`) and negative integers moving the card back. The
    card will simply go in the first box or graduate if the provided `Int`
    results in a new box less than zero or greater than the number of boxes.
    `MoveBoxes 0` is identical to `Pass`.
  - `BackToFirstBox` -- Reset the card fully and begins learning anew.

-}
type Answer
    = Correct
    | Incorrect
    | Pass
    | MoveBoxes Int
    | BackToFirstBox


{-| `answerCardInDeck` functions analogously to `answerCard` but handles maintenance of the `Deck`, which is typically what one would desire. When a card is presented to the user and answered, `answerCardInDeck` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, the index of the card in the `Deck` (e.g. what is returned by the `getDueCardIndices` function), and the `Deck` itself. It returns the updated `Deck`. Use this function if you simply want to store a `Deck` and not worry about updating it manually (which is most likely what you want). Otherwise, use `answerCard` to handle updating the `Deck` manually. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. Note that if an invalid (out of bounds) index is passed, the `Deck` is returned unaltered.
-}
answerCardInDeck : Time.Posix -> Answer -> Int -> Deck a b -> Deck a b
answerCardInDeck time answer index deck =
    { deck
        | cards =
            ArrayX.update index (answerCard time answer deck.settings) deck.cards
    }


{-| When a card is presented to the user and answered, `answerCard` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, and `LeitnerSettings`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`, which is much more convenient. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases.
-}
answerCard : Time.Posix -> Answer -> LeitnerSettings -> Card a -> Card a
answerCard time answer { onIncorrect, numBoxes } card =
    let
        currentBox : Natural
        currentBox =
            case card.srsData of
                New ->
                    Natural.nil

                BoxN { box } ->
                    box

                Graduated ->
                    Natural.succ <| highestBoxIndex numBoxes

        setBox : Natural -> Card a
        setBox n =
            if Natural.compare n (highestBoxIndex numBoxes) == GT then
                { card | srsData = Graduated }

            else
                { card | srsData = BoxN { box = n, lastReviewed = time } }
    in
    case answer of
        Correct ->
            setBox <| Natural.succ currentBox

        Incorrect ->
            case onIncorrect of
                BackOneBox ->
                    setBox <| Natural.pred currentBox

                BackToStart ->
                    setBox Natural.nil

        Pass ->
            setBox currentBox

        MoveBoxes i ->
            Natural.toInt currentBox
                |> (+) i
                |> Natural.fromInt
                |> Maybe.withDefault Natural.nil
                |> setBox

        BackToFirstBox ->
            setBox Natural.nil


{-| `getDueCardIndices` takes settings (`LeitnerSettings`), the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the indices of the subset of the `Deck` that is due for review (as `List Int`). The returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Any new cards in the deck (never having been studied before).

`getDueCardIndices` assumes that a new day begins after 12 hours, e.g. if a card is scheduled to be studied the next day, it will come due after 12 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

-}
getDueCardIndices : Time.Posix -> Deck a b -> List Int
getDueCardIndices time deck =
    Array.toIndexedList deck.cards
        |> List.filter
            (isDue deck.settings time << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compareDue deck.settings time c1 c2)
        |> ListX.reverseMap Tuple.first


{-| `getDueCardIndicesWithDetails` takes settings (`LeitnerSettings`), the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the subset of the `Deck` that is due for review (as a list of records), providing their index and which queue they are currently in, with any relevant queue details. The returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Any new cards in the deck (never having been studied before).

`getDueCardIndicesWithDetails` assumes that a new day begins after 12 hours, e.g. if a card is scheduled to be studied the next day, it will come due after 12 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

-}
getDueCardIndicesWithDetails :
    Time.Posix
    -> Deck a b
    -> List { index : Int, queueDetails : QueueDetails }
getDueCardIndicesWithDetails time deck =
    Array.toIndexedList deck.cards
        |> List.filter
            (isDue deck.settings time << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compareDue deck.settings time c1 c2)
        |> ListX.reverseMap
            (\( index, card ) ->
                { index = index, queueDetails = getQueueDetails card }
            )


{-| `QueueDetails` represents the current status of a card.

  - `NewCard` -- A card that has never before been studied (encountered) by the user.
  - `InBox {...}` -- A card that is being reviewed for retention.
      - `lastReviewed : Time.Posix` -- The date and time the card was last reviewed.
      - `boxNumber : Int` -- The "box" that the card is currently in (starting from `0`).
  - `GraduatedCard` -- A card that has been successfully graduated and thus is no longer being studied.

-}
type QueueDetails
    = NewCard
    | InBox { boxNumber : Int, lastReviewed : Time.Posix }
    | GraduatedCard


{-| `getCardDetails` returns the current queue status for a given card. If you require this for every due card, simply use `getDueCardIndicesWithDetails`.
-}
getCardDetails : Card a -> { queueDetails : QueueDetails }
getCardDetails c =
    { queueDetails = getQueueDetails c }



-- * Non-exposed only below here


{-| Compare the "due"-ness of two cards at a given time.
-}
compareDue : LeitnerSettings -> Time.Posix -> Card a -> Card a -> Order
compareDue settings time c1 c2 =
    case ( c1.srsData, c2.srsData ) of
        -- New cards go last
        ( New, New ) ->
            EQ

        ( New, BoxN _ ) ->
            LT

        ( BoxN _, New ) ->
            GT

        ( BoxN b1, BoxN b2 ) ->
            if
                overdueAmount settings time b1.lastReviewed b1.box
                    >= overdueAmount settings time b2.lastReviewed b2.box
            then
                GT

            else
                LT

        ( Graduated, Graduated ) ->
            EQ

        ( _, Graduated ) ->
            GT

        -- If neither is new, then rank "more due" cards first (by proportion past due).  EQ case doesn't matter, since order becomes irrelevant then.
        ( Graduated, _ ) ->
            LT


{-| Given a card, return its current box status.
-}
getQueueDetails : Card a -> QueueDetails
getQueueDetails c =
    case c.srsData of
        New ->
            NewCard

        BoxN { box, lastReviewed } ->
            InBox
                { boxNumber = Natural.toInt box
                , lastReviewed = lastReviewed
                }

        Graduated ->
            GraduatedCard


{-| Check if a card is currently due to be studied.
-}
isDue : LeitnerSettings -> Time.Posix -> Card a -> Bool
isDue settings time { srsData } =
    case srsData of
        New ->
            True

        BoxN { box, lastReviewed } ->
            overdueAmount settings time lastReviewed box >= 1

        Graduated ->
            False


{-| Given the current time, the time a card was last reviewed, and the box
number, determine what proportion overdue the card is, i.e. `0.9` is 90% of the
way to being overdue and `2` is the interval again overdue.
-}
overdueAmount : LeitnerSettings -> Time.Posix -> Time.Posix -> Natural -> Float
overdueAmount { boxSpacing } time lastReviewed box =
    let
        boxInterval : Int
        boxInterval =
            if box == Natural.nil then
                1

            else
                Natural.toInt box
                    |> boxSpacing
                    |> max 1

        dayDiff : Float
        dayDiff =
            toFloat (diff Hour Time.utc lastReviewed time) / 24
    in
    -- The "next day" starts after 12 hours; this is ultimately a hack to prevent the user of the module from having to determine when the day rolls over.
    (dayDiff + 0.5) / toFloat boxInterval
