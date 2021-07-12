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
import SpacedRepetition.Internal.SMTwo
    exposing
        ( EFactor
        , ReviewHistory(..)
        , Streak(..)
        , eFactor
        , eFactorToFloat
        , streakToInterval
        )
import Time
import Time.Extra exposing (Interval(..), diff)


{-| A `Card` represents a single question or unit of knowledge the user will review. The algorithm specifies that knowledge should be split into "smallest possible items," with each of these being a `Card`; in general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields for a use case may simply be included in the record, e.g.:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

A `Card` contains only the information necessary for scheduling and nothing else; all other information should be added as in the above example.

-}
type alias Card a =
    { a
        | srsData : SRSData
    }


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

        Reviewed eF priorDate streak ->
            Encode.object
                [ ( "eFactor", encoderEFactor eF )
                , ( "priorDate", Encode.int <| Time.posixToMillis priorDate // 1000 ) -- Encode time in seconds; loss of precision is acceptable
                , ( "streak", encoderStreak streak )
                ]

        Repeating eF streak ->
            Encode.object
                [ ( "eFactor", encoderEFactor eF )
                , ( "streak", encoderStreak streak )
                ]


{-| `decoderSRSData` provides a Json decoder for decoding `SRSData` for a `Card`.
-}
decoderSRSData : Decode.Decoder SRSData
decoderSRSData =
    Decode.oneOf
        [ Decode.null New
        , Decode.map3 Reviewed
            (Decode.map eFactor <| Decode.field "eFactor" Decode.float)
            (Decode.map (\t -> Time.millisToPosix <| t * 1000) <| Decode.field "priorDate" Decode.int)
            (Decode.field "streak" decoderStreak)
        , Decode.map2 Repeating
            (Decode.map eFactor <| Decode.field "eFactor" Decode.float)
            (Decode.field "streak" decoderStreak)
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
    = Perfect
    | CorrectWithHesitation
    | CorrectWithDifficulty
    | IncorrectButRemembered
    | IncorrectButFamiliar
    | NoRecollection


{-| `answerCardInDeck` functions analogously to `answerCard` but handles maintenance of the `Deck`, which is typically what one would desire. When a card is presented to the user and answered, `answerCardInDeck` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, the index of the card in the `Deck`, and the `Deck` itself. It returns the updated `Deck`. Use this function if you simply want to store a `Deck` and not worry about updating it manually (which is most likely what you want). Otherwise, use `answerCard` to handle updating the `Deck` manually. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. Note that if an invalid (out of bounds) index is passed, the `Deck` is returned unaltered.
-}
answerCardInDeck : Time.Posix -> Answer -> Int -> Deck a -> Deck a
answerCardInDeck time answer i deck =
    ArrayX.update i (answerCard time answer) deck


{-| When a card is presented to the user and answered, `answerCard` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and an `Answer`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases.
-}
answerCard : Time.Posix -> Answer -> Card a -> Card a
answerCard time answer card =
    updateEFactor answer card
        |> scheduleCard time answer


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
            (\c1 c2 -> sortDue time (Tuple.second c1) (Tuple.second c2))
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
            (\c1 c2 -> sortDue time (Tuple.second c1) (Tuple.second c2))
        |> ListX.reverseMap
            (\( index, card ) ->
                { index = index, queueDetails = getQueueDetails card }
            )


{-| `QueueDetails` represents the current status of a card.

  - `NewCard` -- A card that has never before been studied (encountered) by the user.
  - `ReviewQueue {...}` -- A card that is being reviewed for retention.
      - `lastSeen : Time.Posix` -- The date and time the card was last reviewed.
      - `intervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in.
  - `RepeatingQueue {...}` -- A card to which an unsatisfactory answer was given, slated for review before the end of the session.
      - `intervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in. This will reset to the based interval (1 day) if the card was answered incorrectly.

-}
type QueueDetails
    = NewCard
    | ReviewQueue { lastSeen : Time.Posix, intervalInDays : Int }
    | RepeatingQueue { intervalInDays : Int }


{-| `getCardDetails` returns the current queue status for a given card. If you require this for every due card, simply use `getDueCardIndicesWithDetails`.
-}
getCardDetails : Card a -> { queueDetails : QueueDetails }
getCardDetails c =
    { queueDetails = getQueueDetails c }



-- * Non-exposed only below here


getQueueDetails : Card a -> QueueDetails
getQueueDetails c =
    case c.srsData of
        New ->
            NewCard

        Reviewed _ priorDate streak ->
            ReviewQueue
                { lastSeen = priorDate
                , intervalInDays = streakToInterval streak
                }

        Repeating _ streak ->
            RepeatingQueue { intervalInDays = streakToInterval streak }


sortDue : Time.Posix -> Card a -> Card a -> Order
sortDue time c1 c2 =
    case ( c1.srsData, c2.srsData ) of
        -- New cards go last
        ( New, New ) ->
            EQ

        ( New, _ ) ->
            LT

        ( _, New ) ->
            GT

        -- Repeating cards go before new but after reviewing
        ( Repeating _ _, Repeating _ _ ) ->
            EQ

        ( Repeating _ _, _ ) ->
            LT

        ( _, Repeating _ _ ) ->
            GT

        -- If neither is end of session, then rank "more due" cards first.  Note that this isn't in the SM-2 algorithm and is just a QoL feature.  EQ case doesn't matter, since order becomes irrelevant then.
        ( Reviewed _ reviewed1 streak1, Reviewed _ reviewed2 streak2 ) ->
            if overdueAmount time reviewed1 (streakToInterval streak1) >= overdueAmount time reviewed2 (streakToInterval streak2) then
                GT

            else
                LT


overdueAmount : Time.Posix -> Time.Posix -> Int -> Float
overdueAmount time reviewed interval =
    let
        dayDiff =
            toFloat (diff Hour Time.utc reviewed time) / 24
    in
    -- The "next day" starts after 12 hours; this is ultimately a hack to prevent the user of the module from having to determine when the day rolls over.
    dayDiff + 0.5 - toFloat interval


isDue : Time.Posix -> Card a -> Bool
isDue time { srsData } =
    case srsData of
        New ->
            True

        Repeating _ _ ->
            True

        Reviewed _ reviewed streak ->
            overdueAmount time reviewed (streakToInterval streak) >= 0


updateCardEFactor : EFactor -> Card a -> Card a
updateCardEFactor eFactor card =
    case card.srsData of
        Reviewed _ date streak ->
            { card | srsData = Reviewed eFactor date streak }

        _ ->
            -- Only update e-factor if reviewing
            card


updateEFactor : Answer -> Card a -> Card a
updateEFactor answer card =
    let
        q : Float
        q =
            case answer of
                Perfect ->
                    5

                CorrectWithHesitation ->
                    4

                CorrectWithDifficulty ->
                    3

                IncorrectButRemembered ->
                    2

                IncorrectButFamiliar ->
                    1

                NoRecollection ->
                    0

        oldEFactor =
            case card.srsData of
                Reviewed eF _ _ ->
                    eFactorToFloat eF

                Repeating eF _ ->
                    eFactorToFloat eF

                _ ->
                    0

        updatedEFactor : Float
        updatedEFactor =
            oldEFactor + (0.1 - (5 - q) * (0.08 + (5 - q) * 0.02))
    in
    updateCardEFactor (eFactor updatedEFactor) card


scheduleCard : Time.Posix -> Answer -> Card a -> Card a
scheduleCard time answer card =
    let
        calcInterval : EFactor -> Streak -> Int
        calcInterval eF streak =
            ceiling <| toFloat (streakToInterval streak) * eFactorToFloat eF

        ( newEF, incrementStreak ) =
            case card.srsData of
                New ->
                    ( eFactor 2.5, One )

                Reviewed eF _ streak ->
                    case streak of
                        Zero ->
                            ( eF, One )

                        One ->
                            ( eF, TwoPlus <| calcInterval eF streak )

                        TwoPlus _ ->
                            ( eF, TwoPlus <| calcInterval eF streak )

                Repeating eF streak ->
                    case streak of
                        Zero ->
                            ( eF, One )

                        One ->
                            ( eF, TwoPlus <| calcInterval eF streak )

                        TwoPlus _ ->
                            ( eF, TwoPlus <| calcInterval eF streak )

        newHistory =
            case answer of
                Perfect ->
                    Reviewed newEF time incrementStreak

                CorrectWithHesitation ->
                    Reviewed newEF time incrementStreak

                CorrectWithDifficulty ->
                    -- Never increment streak for CorrectWithDifficulty because it will get incremented when the card graduates from being repeated.
                    case card.srsData of
                        New ->
                            Repeating newEF Zero

                        Reviewed _ _ streak ->
                            Repeating newEF streak

                        Repeating _ streak ->
                            Repeating newEF streak

                IncorrectButRemembered ->
                    Repeating newEF Zero

                IncorrectButFamiliar ->
                    Repeating newEF Zero

                NoRecollection ->
                    Repeating newEF Zero
    in
    { card | srsData = newHistory }


encoderEFactor : EFactor -> Encode.Value
encoderEFactor eF =
    Encode.float <| eFactorToFloat eF


encoderStreak : Streak -> Encode.Value
encoderStreak streak =
    case streak of
        Zero ->
            Encode.string "Z"

        One ->
            Encode.string "O"

        TwoPlus i ->
            Encode.int i


decoderStreak : Decode.Decoder Streak
decoderStreak =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\string ->
                    case string of
                        "Z" ->
                            Decode.succeed Zero

                        "O" ->
                            Decode.succeed One

                        _ ->
                            Decode.fail "Invalid Streak"
                )
        , Decode.map (TwoPlus << max 6) Decode.int
        ]
