module SpacedRepetition.SMTwo exposing
    ( Card, Deck
    , SRSData, newSRSData
    , encoderSRSData, decoderSRSData
    , Answer(..), answerCardInDeck, answerCard
    , getDueCardIndices, getDueCardIndicesWithDetails
    , QueueDetails(..), getCardDetails
    )

{-| This package provides everything necessary to create spaced repetition software using the SM-2 algorithm. The SM-2 algorithm was one of the earliest computerized implementations of a spaced repetition algorithm (created in 1988 by Piotr Wozniak) and has been released for free public use when accompanied by the following notice:

**Algorithm SM-2, (C) Copyright SuperMemo World, 1991.**

  - <http://www.supermemo.com>
  - <http://www.supermemo.eu>

For details about this algorithm, please refer to the [following description](https://www.supermemo.com/en/archives1990-2015/english/ol/sm2), written by its creator.

This package differs from the reference implementation of the SM-2 algorithm by sorting due items in decreasing severity of being due (i.e. more overdue items will be presented first).


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

The SM-2 algorithm depends on grading answers on a scale from 0-5, with 3 incorrect and 3 correct responses. For some use cases, it may be able to programmatically determine the `Answer` from a user's response. In other cases, however, the user may need to self-report.

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
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import SpacedRepetition.Internal.SMTwo
    exposing
        ( EFactor
        , ReviewHistory(..)
        , Streak(..)
        , decodeEFactor
        , decodeStreak
        , defaultEFactor
        , eFactor
        , eFactorToFloat
        , encodeEFactor
        , encodeStreak
        , streakToInterval
        )
import SpacedRepetition.Internal.Time as Time
import Time
import Time.Extra exposing (Interval(..), diff)


{-| A `Card` represents a single question or unit of knowledge the user will review. The algorithm specifies that knowledge should be split into "smallest possible items," with each of these being a `Card`; in general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields for a use case may simply be included in the record, e.g.:

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


{-| `SRSData` contains all data necessary for the SM-2 scheduling algorithm and may be created with the `newSRSData` function. It may additionally be saved/loaded using the Json encoder/decoder in this package
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

        Repeating { ease, streak } ->
            Encode.object
                [ ( "eFactor", encodeEFactor ease )
                , ( "streak", encodeStreak streak )
                ]

        Reviewed { ease, lastReviewed, streak } ->
            Encode.object
                [ ( "eFactor", encodeEFactor ease )
                , ( "priorDate", Time.encode lastReviewed )
                , ( "streak", encodeStreak streak )
                ]


{-| `decoderSRSData` provides a Json decoder for decoding `SRSData` for a `Card`.
-}
decoderSRSData : Decode.Decoder SRSData
decoderSRSData =
    Decode.oneOf
        [ Decode.null New
        , Decode.map3
            (\ease lastReviewed streak ->
                Reviewed
                    { ease = ease
                    , lastReviewed = lastReviewed
                    , streak = streak
                    }
            )
            (Decode.field "eFactor" decodeEFactor)
            (Decode.field "priorDate" Time.decode)
            (Decode.field "streak" decodeStreak)
        , Decode.map2 (\ease streak -> Repeating { ease = ease, streak = streak })
            (Decode.field "eFactor" decodeEFactor)
            (Decode.field "streak" decodeStreak)
        ]


{-| The `Answer` type represents how accurate/certain a user's response was to a card and must be passed to `answerCard` whenever a `Card` is reviewed. This package strives to provide type names that are generally sensible/understandable, but the slightly more explanatory descriptions provided by the creator of the algorithm are presented below:

  - `Perfect` -- "perfect response"

  - `CorrectWithHesitation` -- "correct response after a hesitation"

  - `CorrectWithDifficulty` -- "correct response recalled with serious difficulty"

  - `IncorrectButRemembered` -- "incorrect response; where the correct one seemed easy to recall"

  - `IncorrectButFamiliar` -- "incorrect response; the correct one remembered"

  - `NoRecollection` -- "complete blackout"

-}
type Answer
    = CorrectWithDifficulty
    | CorrectWithHesitation
    | IncorrectButFamiliar
    | IncorrectButRemembered
    | NoRecollection
    | Perfect


{-| `answerCardInDeck` functions analogously to `answerCard` but handles maintenance of the `Deck`, which is typically what one would desire. When a card is presented to the user and answered, `answerCardInDeck` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, the index of the card in the `Deck`, and the `Deck` itself. It returns the updated `Deck`. Use this function if you simply want to store a `Deck` and not worry about updating it manually (which is most likely what you want). Otherwise, use `answerCard` to handle updating the `Deck` manually. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. Note that if an invalid (out of bounds) index is passed, the `Deck` is returned unaltered.
-}
answerCardInDeck : Time.Posix -> Answer -> Int -> Deck a -> Deck a
answerCardInDeck time answer i =
    ArrayX.update i (answerCard time answer)


{-| When a card is presented to the user and answered, `answerCard` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and an `Answer`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases.
-}
answerCard : Time.Posix -> Answer -> Card a -> Card a
answerCard time answer =
    updateEFactor answer
        >> scheduleCard time answer


{-| `getDueCardIndices` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the indices of the subset of the `Deck` that is due for review (as `List Int`). While the SM-2 algorithm does not specify this, the returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (in number of days)
    2.  Cards less overdue (in number of days)
2.  Cards to be repeated at the end of the current session (due to poor-quality answers)
3.  Any new cards in the deck (never having been studied before).

`getDueCardIndices` assumes that a new day begins after 12 hours, e.g. if a card is scheduled to be studied the next day, it will become due after 12 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

-}
getDueCardIndices : Time.Posix -> Deck a -> List Int
getDueCardIndices time deck =
    Array.toIndexedList deck
        |> List.filter
            (isDue time << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compareDue time c1 c2)
        |> ListX.reverseMap Tuple.first


{-| `getDueCardIndicesWithDetails` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the subset of the `Deck` that is due for review (as a list of records), providing their index and which queue they are currently in, with any relevant queue details. While the SM-2 algorithm does not specify this, the returned indices will be sorted in the following order:

1.  Cards overdue for review
    1.  Cards more overdue (in number of days)
    2.  Cards less overdue (in number of days)
2.  Cards to be repeated at the end of the current session (due to poor-quality answers)
3.  Any new cards in the deck (never having been studied before).

`getDueCardIndicesWithDetails` assumes that a new day begins after 12 hours, e.g. if a card is scheduled to be studied the next day, it will become due after 12 hours of elapsed time. This can of course create edge cases where cards are reviewed too "early" if one studies very early in the morning and again late at night. Still, only very "new" cards would be affected, in which case the adverse effect is presumably minimal.

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
      - `intervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in.
      - `lastReviewed : Time.Posix` -- The date and time the card was last reviewed.
  - `RepeatingQueue {...}` -- A card to which an unsatisfactory answer was given, slated for review before the end of the session.
      - `intervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in. This will reset to the based interval (1 day) if the card was answered incorrectly.

-}
type QueueDetails
    = NewCard
    | RepeatingQueue { intervalInDays : Int }
    | ReviewQueue { intervalInDays : Int, lastReviewed : Time.Posix }


{-| `getCardDetails` returns the current queue status for a given card. If you require this for every due card, simply use `getDueCardIndicesWithDetails`.
-}
getCardDetails : Card a -> { queueDetails : QueueDetails }
getCardDetails c =
    { queueDetails = getQueueDetails c }



-- * Non-exposed only below here


{-| Given a card, return its review status.
-}
getQueueDetails : Card a -> QueueDetails
getQueueDetails c =
    case c.srsData of
        New ->
            NewCard

        Repeating { streak } ->
            RepeatingQueue { intervalInDays = Natural.toInt <| streakToInterval streak }

        Reviewed { lastReviewed, streak } ->
            ReviewQueue
                { intervalInDays = Natural.toInt <| streakToInterval streak
                , lastReviewed = lastReviewed
                }


{-| Compare the "due"-ness of two cards at a given time.
-}
compareDue : Time.Posix -> Card a -> Card a -> Order
compareDue time c1 c2 =
    case ( c1.srsData, c2.srsData ) of
        -- New cards go last
        ( New, New ) ->
            EQ

        ( New, _ ) ->
            LT

        ( _, New ) ->
            GT

        -- Repeating cards go before new but after reviewing
        ( Repeating _, Repeating _ ) ->
            EQ

        ( Repeating _, _ ) ->
            LT

        ( _, Repeating _ ) ->
            GT

        -- If neither is end of session, then rank "more due" cards first.  Note that this isn't in the SM-2 algorithm and is just a QoL feature.  EQ case doesn't matter, since order becomes irrelevant then.
        ( Reviewed r1, Reviewed r2 ) ->
            if
                daysOverdue time r1.lastReviewed (streakToInterval r1.streak)
                    >= daysOverdue time r2.lastReviewed (streakToInterval r2.streak)
            then
                GT

            else
                LT


{-| Given the current time, the time a card was last reviewed, and scheduled
interval, determine how many days overdue the card is.
-}
daysOverdue : Time.Posix -> Time.Posix -> Natural -> Float
daysOverdue time reviewed interval =
    let
        dayDiff : Float
        dayDiff =
            toFloat (diff Hour Time.utc reviewed time) / 24
    in
    -- The "next day" starts after 12 hours; this is ultimately a hack to prevent the user of the module from having to determine when the day rolls over.
    dayDiff + 0.5 - Natural.toFloat interval


{-| Check if a card is currently due to be studied.
-}
isDue : Time.Posix -> Card a -> Bool
isDue time { srsData } =
    case srsData of
        New ->
            True

        Repeating _ ->
            True

        Reviewed { lastReviewed, streak } ->
            daysOverdue time lastReviewed (streakToInterval streak) >= 0


{-| Given an answer quality, update the ease of a card (if applicable).
-}
updateEFactor : Answer -> Card a -> Card a
updateEFactor answer card =
    let
        q : Float
        q =
            case answer of
                CorrectWithDifficulty ->
                    3

                CorrectWithHesitation ->
                    4

                IncorrectButFamiliar ->
                    1

                IncorrectButRemembered ->
                    2

                NoRecollection ->
                    0

                Perfect ->
                    5

        oldEFactor : Float
        oldEFactor =
            case card.srsData of
                Repeating { ease } ->
                    eFactorToFloat ease

                Reviewed { ease } ->
                    eFactorToFloat ease

                _ ->
                    0

        updatedEFactor : Float
        updatedEFactor =
            oldEFactor + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))
    in
    case card.srsData of
        Reviewed { lastReviewed, streak } ->
            { card
                | srsData =
                    Reviewed
                        { ease = eFactor updatedEFactor
                        , lastReviewed = lastReviewed
                        , streak = streak
                        }
            }

        _ ->
            -- Only update e-factor if reviewing
            card


{-| Given an answer quality, schedule the card for future review, updating its queue status.
-}
scheduleCard : Time.Posix -> Answer -> Card a -> Card a
scheduleCard time answer card =
    let
        continueStreak : EFactor -> Streak -> Streak
        continueStreak eF streak =
            (Natural.toFloat (streakToInterval streak)
                * eFactorToFloat eF
            )
                |> ceiling
                |> Natural.fromInt
                |> Maybe.withDefault Natural.nil
                |> (\interval -> TwoPlus { interval = interval })

        ( newEF, incrementStreak ) =
            case card.srsData of
                New ->
                    ( defaultEFactor, One )

                Repeating { ease, streak } ->
                    case streak of
                        One ->
                            ( ease, continueStreak ease streak )

                        TwoPlus _ ->
                            ( ease, continueStreak ease streak )

                        Zero ->
                            ( ease, One )

                Reviewed { ease, streak } ->
                    case streak of
                        One ->
                            ( ease, continueStreak ease streak )

                        TwoPlus _ ->
                            ( ease, continueStreak ease streak )

                        Zero ->
                            ( ease, One )

        newHistory : ReviewHistory
        newHistory =
            let
                breakStreak : ReviewHistory
                breakStreak =
                    Repeating { ease = newEF, streak = Zero }
            in
            case answer of
                CorrectWithDifficulty ->
                    case card.srsData of
                        New ->
                            breakStreak

                        Repeating r ->
                            Repeating { r | ease = newEF }

                        Reviewed { streak } ->
                            Repeating { ease = newEF, streak = streak }

                CorrectWithHesitation ->
                    Reviewed { ease = newEF, lastReviewed = time, streak = incrementStreak }

                IncorrectButFamiliar ->
                    breakStreak

                IncorrectButRemembered ->
                    breakStreak

                NoRecollection ->
                    breakStreak

                Perfect ->
                    Reviewed { ease = newEF, lastReviewed = time, streak = incrementStreak }
    in
    { card | srsData = newHistory }
