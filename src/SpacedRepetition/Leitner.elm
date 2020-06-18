module SpacedRepetition.Leitner exposing
    ( LeitnerSettings, OnIncorrect(..), SpacingFunction, doubleSpacing, fibonacciSpacing, numberOfBoxes
    , Card, Deck
    , SRSData, newSRSData
    , encoderSRSData, decoderSRSData
    , Answer(..), answerCardInDeck, answerCard
    , getDueCardIndices
    )

{-| This package provides everything necessary to create spaced repetition software using a variant of the Leitner system. The Leitner system was proposed by Sebastian Leitner in the early 1970s and was originally intended for use with physical (paper) flashcards.

For the basics about this algorithm, please refer to the [following description](https://en.wikipedia.org/wiki/Leitner_system) on Wikipedia.


# Settings

This algorithm requires certain settings be provided when functions are called to specify the behavior of the (very general) system.

@docs LeitnerSettings, OnIncorrect, SpacingFunction, doubleSpacing, fibonacciSpacing, numberOfBoxes


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

@docs getDueCardIndices

-}

import Array exposing (Array)
import Array.Extra as A
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import SpacedRepetition.Internal.Leitner exposing (Box(..), NumberOfBoxes(..))
import Time
import Time.Extra exposing (Interval(..), diff)


{-| `LeitnerSettings` customizes the behavior of this algorithm. Three parameters must be defined: the behavior of the system upon an incorrect answer, the spacing (interval) between "boxes", and the total number of boxes before a card "graduates." No builder functions are provided, as only three settings exist and the Leitner system doesn't have "defaults" to speak of.
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


{-| `SpacingFunction` takes an integer argument, representing the box number, and returns an integer representing the interval in days that cards in that box should go between reviews. Note that box numbering starts at `0`, and intervals will always be at least 1 day (regardless of the output of a `SpacingFunction`). A custom function may be provided, or the pre-made functions `doubleSpacing` or `fibonacciSpacing` may be used. For obvious reasons, care should be taken that the complexity/recursive depth/etc. does not become excessive within the number of boxes your system will use.
-}
type alias SpacingFunction =
    Int -> Int


{-| `doubleSpacing` is a `SpacingFunction` with which the interval between each box simply doubles, e.g. Box 0 has an interval of `1`, Box 1 of `2`, Box 2 of `4`, Box 3 of `8`, etc.
-}
doubleSpacing : SpacingFunction
doubleSpacing boxNumber =
    -- Box Numbering starts at 0
    2 ^ boxNumber


{-| `fibonacciSpacing` is a `SpacingFunction` with which the interval between each box follows the Fibonacci sequence, e.g. Box 0 has an interval of `1`, Box 1 of `1`, Box 2 of `2`, Box 3 of `3`, Box 4 of `5`, Box 5 of `8`, Box 6 of `13`, etc. Note that the interval caps at Box 18, with an interval of more than 10 years (4181 days). A 4181 interval will be used for all boxes from 18 on.
-}
fibonacciSpacing : SpacingFunction
fibonacciSpacing box =
    case box of
        0 ->
            1

        1 ->
            1

        _ ->
            if box < 0 then
                1

            else if box < 19 then
                fibonacciSpacing (box - 1) + fibonacciSpacing (box - 2)

            else
                -- Cap once we cross 10 years, because does anyone really need longer intervals than that?
                4181


{-| `numberOfBoxes` may be used to specify the total number of boxes before a card "graduates" (i.e. is no longer reviewed). It takes an integer as a parameter, specifying a system with that integer number of boxes. There must, of course, be at least 1 box in the system (and there should almost certainly be more).
-}
numberOfBoxes : Int -> NumberOfBoxes
numberOfBoxes i =
    NumberOfBoxes <| max 1 i


{-| A `Card` represents a single question or unit of knowledge that the user will review. In general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields are required for a use case may simply be included in the record, e.g.:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

A `Card` by default contains only the information necessary for scheduling and nothing else; all other information should be added as in the above example.

-}
type alias Card a =
    { a
        | srsData : SRSData
    }


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

        Graduated ->
            Encode.string "G"

        BoxN box date ->
            Encode.object
                [ ( "boxNum", Encode.int box )
                , ( "reviewDate", Encode.int <| Time.posixToMillis date // 1000 ) -- Encode time in seconds; loss of precision is acceptable
                ]


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
        , Decode.map2 BoxN
            (Decode.field "boxNum" Decode.int)
            (Decode.map (\t -> Time.millisToPosix <| t * 1000) <|
                Decode.field "reviewDate" Decode.int
            )
        ]


{-| `Answer` must be passed to `answerCard`/`answerCardInDeck` when the user answers a card. It is usually best to simply use `Correct` and `Incorrect` (which follow the behavior specified in `LeitnerSettings`), but one could potentially provide the user with more options. `Pass` will leave the card in its current box. `MoveBoxes` allows one to specify the number of boxes to move the card, with positive integers advancing the card (typically meaning longer intervals, depending on the `SpacingFunction`) and negative integers moving the card back. The card will simply go in the first box or graduate if the provided `Int` results in a new box less than zero or greater than the number of boxes. `MoveBoxes 0` is identical to `Pass`. `BackToFirstBox` resets the card fully and begins learning anew.
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
            A.update index (answerCard time answer deck.settings) deck.cards
    }


{-| When a card is presented to the user and answered, `answerCard` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, and `LeitnerSettings`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`, which is much more convenient. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases.
-}
answerCard : Time.Posix -> Answer -> LeitnerSettings -> Card a -> Card a
answerCard time answer { onIncorrect, numBoxes } card =
    let
        currentBox =
            case card.srsData of
                New ->
                    0

                Graduated ->
                    maxBoxes

                BoxN boxNum _ ->
                    -- Things that are over max boxes or <0 will get fixed when answered.
                    clamp 0 maxBoxes boxNum

        maxBoxes =
            case numBoxes of
                NumberOfBoxes i ->
                    i

        clampBox newBox =
            if newBox >= maxBoxes then
                Graduated

            else
                BoxN (max 0 newBox) time

        changeBox amount =
            { card | srsData = clampBox <| currentBox + amount }

        firstBox =
            { card | srsData = BoxN 0 time }
    in
    case answer of
        Correct ->
            changeBox 1

        Incorrect ->
            case onIncorrect of
                BackOneBox ->
                    changeBox -1

                BackToStart ->
                    firstBox

        Pass ->
            -- Passing on a card still puts it in the first box if it's New
            changeBox 0

        MoveBoxes i ->
            changeBox i

        BackToFirstBox ->
            firstBox


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
            (\c1 c2 -> sortDue deck.settings time (Tuple.second c1) (Tuple.second c2))
        |> List.Extra.reverseMap Tuple.first



-- * Non-exposed only below here


sortDue : LeitnerSettings -> Time.Posix -> Card a -> Card a -> Order
sortDue settings time c1 c2 =
    case ( c1.srsData, c2.srsData ) of
        -- New cards go last
        ( Graduated, Graduated ) ->
            EQ

        ( _, Graduated ) ->
            GT

        ( Graduated, _ ) ->
            LT

        ( New, New ) ->
            EQ

        ( New, BoxN _ _ ) ->
            LT

        ( BoxN _ _, New ) ->
            GT

        -- If neither is new, then rank "more due" cards first (by proportion past due).  EQ case doesn't matter, since order becomes irrelevant then.
        ( BoxN box1 reviewedOn1, BoxN box2 reviewedOn2 ) ->
            if overdueAmount settings time reviewedOn1 box1 >= overdueAmount settings time reviewedOn2 box2 then
                GT

            else
                LT


isDue : LeitnerSettings -> Time.Posix -> Card a -> Bool
isDue settings time { srsData } =
    case srsData of
        New ->
            True

        Graduated ->
            False

        BoxN box lastReviewed ->
            overdueAmount settings time lastReviewed box >= 1


overdueAmount : LeitnerSettings -> Time.Posix -> Time.Posix -> Int -> Float
overdueAmount { boxSpacing } time lastReviewed box =
    let
        boxInterval =
            if box > 0 then
                boxSpacing box

            else
                1

        dayDiff : Float
        dayDiff =
            toFloat (diff Hour Time.utc lastReviewed time) / 24
    in
    -- The "next day" starts after 12 hours; this is ultimately a hack to prevent the user of the module from having to determine when the day rolls over.
    (dayDiff + 0.5) / toFloat boxInterval
