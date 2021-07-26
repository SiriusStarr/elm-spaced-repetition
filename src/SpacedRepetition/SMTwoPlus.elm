module SpacedRepetition.SMTwoPlus exposing
    ( Card, Deck
    , SRSData, newSRSData
    , encoderSRSData, decoderSRSData
    , PerformanceRating, performanceRating, answerCardInDeck, answerCard, IncorrectSchedulingFunction, oneMinusReciprocalDiffWeightSquared
    , getDueCardIndices, getDueCardIndicesWithDetails
    , QueueDetails(..), getCardDetails
    )

{-| This package provides everything necessary to create spaced repetition software using the SM2+ algorithm. The SM2+ algorithm was proposed by "BlueRaja" as an improvement of the SM-2 algorithm, which has been released for free public use when accompanied by the following notice:

**Algorithm SM-2, (C) Copyright SuperMemo World, 1991.**

  - <http://www.supermemo.com>
  - <http://www.supermemo.eu>

For details about the SM2+ algorithm and its purported advantages over the SM-2 algorithm, please refer to the [following blog post](http://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2).

**It should be noted that this algorithm produces seemingly illogical behavior, namely that more incorrect answers result in longer intervals than less incorrect answers. Do not use it if this behavior is objectionable to you (as it probably should be).** A custom scheduling function for incorrect answers may be provided if one still wishes to use it (with a different incorrect scheduler).

Most notably (versus SM-2), the SM2+ algorithm "rewards" correctly answering more-overdue cards and sorts due items based on the proportion they are overdue, not the absolute amount they are overdue by. Additionally, the initial intervals are 1 day -> 3 days, rather than 1 day -> 6 days.


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

The SM2+ algorithm depends on grading answers on a scale from 0.0 to 1.0, with answers of 0.6 and above being correct (and incorrect below that) responses. For some use cases, it may be able to programmatically determine the `PerformanceRating` of a user's response. In other cases, however, the user may need to self-report.

@docs PerformanceRating, performanceRating, answerCardInDeck, answerCard, IncorrectSchedulingFunction, oneMinusReciprocalDiffWeightSquared


# Due Cards

Besides answering cards, this package handles determining which cards in a `Deck` are due and require answering.

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
import SpacedRepetition.Internal.SMTwoPlus
    exposing
        ( Difficulty
        , Interval
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
        , performanceRatingToFloat
        )
import SpacedRepetition.Internal.Time as Time
import Time
import Time.Extra as TimeExtra


{-| A `Card` represents a single question or unit of knowledge the user will review. In general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields for a use case may simply be included in the record, e.g.:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

A `Card` contains only the information necessary for scheduling and nothing else; all other information should be added as in the above example.

-}
type alias Card a =
    { a | srsData : SRSData }


{-| A `Deck` represents a list of cards to be studied (this might be called a "collection" in other software). It is simply an `Array` of `Card` and requires no special creation or manipulation. Maintaining the state of a `Deck` may be handled by the user of the module or by this module itself. In general, it is probably best not to add a massive quantity of new (unstudied) cards to a deck at once.
-}
type alias Deck a =
    Array (Card a)


{-| `SRSData` contains all data necessary for the SM2+ scheduling algorithm and may be created with the `newSRSData` function. It may additionally be saved/loaded using the Json encoder/decoder in this package
-}
type alias SRSData =
    ReviewHistory


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

        Reviewed { difficulty, interval, lastReviewed } ->
            Encode.object
                [ ( "dateLastReviewed", Time.encode lastReviewed )
                , ( "difficulty", encodeDifficulty difficulty )
                , ( "daysBetweenReviews", encodeInterval interval )
                ]


{-| `decoderSRSData` provides a Json decoder for decoding `SRSData` for a `Card`.
-}
decoderSRSData : Decode.Decoder SRSData
decoderSRSData =
    Decode.oneOf
        [ Decode.null New
        , Decode.map3
            (\difficulty interval lastReviewed ->
                Reviewed
                    { difficulty = difficulty
                    , interval = interval
                    , lastReviewed = lastReviewed
                    }
            )
            (Decode.field "difficulty" decodeDifficulty)
            (Decode.field "daysBetweenReviews" decodeInterval)
            (Decode.field "dateLastReviewed" Time.decode)
        ]


{-| The `PerformanceRating` type represents how accurate/certain a user's response was to a card and must be passed to `answerCard` whenever a `Card` is answered. `PerformanceRating` is quantitative and must be between 0.0 and 1.0, with values of 0.6 and greater representing a "correct" answer. A `PerformanceRating` may be created with the `performanceRating` function.
-}
type alias PerformanceRating =
    SpacedRepetition.Internal.SMTwoPlus.PerformanceRating


{-| The `performanceRating` function creates a `PerformanceRating`. `PerformanceRating` is quantitative and must be between 0.0 and 1.0, with values of 0.6 and greater representing a "correct" answer.
-}
performanceRating : Float -> PerformanceRating
performanceRating =
    SpacedRepetition.Internal.SMTwoPlus.performanceRating


{-| `answerCardInDeck` functions analogously to `answerCard` but handles maintenance of the `Deck`, which is typically what one would desire. When a card is presented to the user and answered, `answerCardInDeck` should be called with a `Maybe IncorrectSchedulingFunction`, the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), a `PerformanceRating`, the index of the card in the `Deck`, and the `Deck` itself. It returns the updated `Deck`. Use this function if you simply want to store a `Deck` and not worry about updating it manually (which is most likely what you want). Otherwise, use `answerCard` to handle updating the `Deck` manually. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. Note that if an invalid (out of bounds) index is passed, the `Deck` is returned unaltered. `IncorrectSchedulingFunction` may be provided to fix the issue with scheduling incorrect cards inherent in the algorithm. If `Nothing` is provided, the algorithm-specified `1 / diffWeight ^ 2` scaling is used that results in questionable behavior.
-}
answerCardInDeck : Maybe IncorrectSchedulingFunction -> Time.Posix -> PerformanceRating -> Int -> Deck a -> Deck a
answerCardInDeck scheduleFunc time perf i deck =
    ArrayX.update i (answerCard scheduleFunc time perf) deck


{-| When a card is presented to the user and answered, `answerCard` should be called with a `Maybe IncorrectSchedulingFunction`, the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and an `PerformanceRating`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. `IncorrectSchedulingFunction` may be provided to fix the issue with scheduling incorrect cards inherent in the algorithm. If `Nothing` is provided, the algorithm-specified `1 / diffWeight ^ 2` scaling is used that results in questionable behavior.
-}
answerCard : Maybe IncorrectSchedulingFunction -> Time.Posix -> PerformanceRating -> Card a -> Card a
answerCard scheduleFunc time perf card =
    let
        percentDue : Float
        percentDue =
            if isCorrect perf then
                case percentOverdue time card of
                    Just f ->
                        min 2 f

                    Nothing ->
                        1

            else
                1
    in
    scheduleCard scheduleFunc time percentDue perf card


{-| `IncorrectSchedulingFunction` must take a float as an argument, representing "difficultyWeight" (which is in the interval [1.3, 3.0]), and return a float representing the factor by which the interval should be scaled (which should probably be in the interval [0.0, 1.0]). This function will only be called with incorrect answers, not correct ones. Note that an incorrect interval cannot be less than 1 day, so any factor resulting in a shorter interval will simply result in an interval of 1 day. A custom function may be provided, or the pre-made function `oneMinusReciprocalDiffWeightSquared` may be used, which seems a likely correction.
-}
type alias IncorrectSchedulingFunction =
    Float -> Float


{-| `oneMinusReciprocalDiffWeightSquared` represents an attempt to "fix" the SM2+ algorithm, scheduling incorrect cards with the more logical behavior of more difficult cards receiving shorter intervals, with the assumption that the original formula `1 / diffWeight ^ 2` was intended to be `1 - 1 / diffWeight ^ 2`. It maps "difficultyWeight" ([1.3, 3.0]) to the interval [0.408, 0.889].
-}
oneMinusReciprocalDiffWeightSquared : Float -> Float
oneMinusReciprocalDiffWeightSquared diffWeight =
    1 - 1 / diffWeight ^ 2


{-| `getDueCardIndices` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the indices of the subset of the `Deck` that is due for review (as `List Int`). The returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Any new cards in the deck (never having been studied before)

`getDueCardIndices` assumes that a new day begins after 8 hours, e.g. if a card is scheduled to be studied the next day, it will become due after 8 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

-}
getDueCardIndices : Time.Posix -> Deck a -> List Int
getDueCardIndices time deck =
    Array.toIndexedList deck
        |> List.filter
            (isDue time << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compareDue time c1 c2)
        |> ListX.reverseMap Tuple.first


{-| `getDueCardIndicesWithDetails` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the the subset of the `Deck` that is due for review (as a list of records), providing their index and which queue they are currently in, with any relevant queue details. The returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Any new cards in the deck (never having been studied before)

`getDueCardIndicesWithDetails` assumes that a new day begins after 8 hours, e.g. if a card is scheduled to be studied the next day, it will become due after 8 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

-}
getDueCardIndicesWithDetails :
    Time.Posix
    -> Deck a
    -> List { index : Int, queueDetails : QueueDetails }
getDueCardIndicesWithDetails time deck =
    Array.toIndexedList deck
        |> List.filter
            (isDue time << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compareDue time c1 c2)
        |> ListX.reverseMap
            (\( index, card ) ->
                { index = index, queueDetails = getQueueDetails card }
            )


{-| `QueueDetails` represents the current status of a card.

  - `NewCard` -- A card that has never before been studied (encountered) by the user.
  - `ReviewQueue {...}` -- A card that is being reviewed for retention.
      - `intervalInDays : Float` -- The interval, in days from the date last seen, that the card is slated for review in.
      - `lastReviewed : Time.Posix` -- The date and time the card was last reviewed.

-}
type QueueDetails
    = NewCard
    | ReviewQueue { intervalInDays : Float, lastReviewed : Time.Posix }


{-| `getCardDetails` returns the current queue status for a given card. If you require this for every due card, simply use `getDueCardIndicesWithDetails`.
-}
getCardDetails : Card a -> { queueDetails : QueueDetails }
getCardDetails c =
    { queueDetails = getQueueDetails c }



-- Non-exposed functions below


{-| Given a card, return its review status.
-}
getQueueDetails : Card a -> QueueDetails
getQueueDetails c =
    case c.srsData of
        New ->
            NewCard

        Reviewed { interval, lastReviewed } ->
            ReviewQueue
                { intervalInDays = intervalToFloat interval
                , lastReviewed = lastReviewed
                }


{-| Compare the "due"-ness of two cards at a given time.
-}
compareDue : Time.Posix -> Card a -> Card a -> Order
compareDue time c1 c2 =
    case ( percentOverdue time c1, percentOverdue time c2 ) of
        -- New cards go last
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, _ ) ->
            LT

        ( _, Nothing ) ->
            GT

        -- If neither is new, then rank "more due" cards first (by percent due).  EQ case doesn't matter, since order becomes irrelevant then.
        ( Just percent1, Just percent2 ) ->
            if percent1 >= percent2 then
                GT

            else
                LT


{-| Check if a card is currently due to be studied.
-}
isDue : Time.Posix -> Card a -> Bool
isDue time card =
    case percentOverdue time card of
        Just f ->
            f >= 1

        Nothing ->
            True


{-| Given possibly a corrected function for scheduling incorrect answers, the
current time, the percent overdue, and the answer quality, schedule a card for
future review, updating its queue status.
-}
scheduleCard : Maybe IncorrectSchedulingFunction -> Time.Posix -> Float -> PerformanceRating -> Card a -> Card a
scheduleCard scheduleFunc time percentDue perf card =
    let
        useFunc : IncorrectSchedulingFunction
        useFunc =
            case scheduleFunc of
                Just func ->
                    func

                Nothing ->
                    \dW -> 1 / (dW ^ 2)
    in
    { card
        | srsData =
            Reviewed
                { difficulty = newDifficulty percentDue perf card
                , interval = nextInterval useFunc percentDue perf card
                , lastReviewed = time
                }
    }


{-| Given the current time, determine what proportion overdue the card is, i.e.
`0.9` is 90% of the way to being overdue and `2` is the interval again overdue,
if possible.
-}
percentOverdue : Time.Posix -> Card a -> Maybe Float
percentOverdue time card =
    case card.srsData of
        New ->
            Nothing

        Reviewed { interval, lastReviewed } ->
            let
                hourDiff : Float
                hourDiff =
                    TimeExtra.diff TimeExtra.Hour Time.utc lastReviewed time
                        -- Ensure that weird edge cases don't happen reviewing before the card was last reviewed
                        |> max 0
                        |> toFloat
            in
            if hourDiff <= 8 then
                -- Per algorithm, ignore cards if it's been less than ~8 hours.
                Just <| min 0.99 <| (hourDiff / 24) / intervalToFloat interval

            else
                Just <| ((hourDiff + 16) / 24) / intervalToFloat interval


{-| Given the percent overdue a card is and an answer quality, determine the new
difficulty of the card.
-}
newDifficulty : Float -> PerformanceRating -> Card a -> Difficulty
newDifficulty percentDue perf card =
    case card.srsData of
        New ->
            defaultDifficulty

        Reviewed { difficulty } ->
            createDifficulty <|
                difficultyToFloat difficulty
                    + percentDue
                    * (8 - 9 * performanceRatingToFloat perf)
                    / 17


{-| Given a way to schedule incorrect cards, a percent overdue, and an answer
quality, determine the next interval for a card.
-}
nextInterval : IncorrectSchedulingFunction -> Float -> PerformanceRating -> Card a -> Interval
nextInterval scheduleFunc percentDue perf card =
    case card.srsData of
        New ->
            createInterval 1

        Reviewed { difficulty, interval } ->
            let
                difficultyWeight : Float
                difficultyWeight =
                    3 - 1.7 * difficultyToFloat difficulty

                multiplier : Float
                multiplier =
                    if isCorrect perf then
                        1 + (difficultyWeight - 1) * percentDue

                    else
                        scheduleFunc difficultyWeight
            in
            createInterval <| intervalToFloat interval * multiplier


{-| Determine if an answer was correct or not.
-}
isCorrect : PerformanceRating -> Bool
isCorrect p =
    0.6 <= performanceRatingToFloat p
