module SpacedRepetition.SMTwoAnki exposing
    ( AnkiSettings, createSettings, setNewSteps, setGraduatingInterval, setEasyInterval, setStartingEase, setEasyBonus, setIntervalModifier, setMaximumInterval, setHardInterval, setLapseSteps, setLapseNewInterval, setLapseMinimumInterval, setLeechThreshold
    , Card, Deck
    , SRSData, newSRSData
    , encoderSRSData, decoderSRSData, encoderAnkiSettings, decoderAnkiSettings
    , Answer(..), answerCardInDeck, answerCard
    , getDueCardIndices, getDueCardIndicesWithDetails
    , QueueDetails(..), getCardDetails, getLeeches
    )

{-| This package provides everything necessary to create spaced repetition software using the algorithm used by the popular F/OSS program Anki. Anki's algorithm is a heavily-modified version of the SM-2 algorithm, which has been released for free public use when accompanied by the following notice:

**Algorithm SM-2, (C) Copyright SuperMemo World, 1991.**

  - <http://www.supermemo.com>
  - <http://www.supermemo.eu>

For details about Anki's algorithm, please refer to [the following section of its manual](https://apps.ankiweb.net/docs/manual.html#what-spaced-repetition-algorithm-does-anki-use).

The above description details how Anki's algorithm differs from the SM-2 algorithm, but briefly, the following differences may be noted:

  - SM-2 defines an initial interval of 1 day then 6 days, whereas Anki's algorithm allows for customization of these initial intervals.

  - Anki's algorithm uses 4 choices (3 during learning) for answering cards, not 6.

  - Answering cards later than scheduled will be factored into the next interval calculation.

  - Failing a review card may cause behavior besides fully resetting it (if desired).

  - "Easy" answer choice increases the next interval in addition to ease/E-factor.

  - Successive failures while cards are in learning do not result in further decreases to the card’s ease.


# Settings

This algorithm requires certain settings be provided when functions are called to specify the behavior of the system. A builder pattern is available for creating settings with defaults.

@docs AnkiSettings, createSettings, setNewSteps, setGraduatingInterval, setEasyInterval, setStartingEase, setEasyBonus, setIntervalModifier, setMaximumInterval, setHardInterval, setLapseSteps, setLapseNewInterval, setLapseMinimumInterval, setLeechThreshold


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

A Json encoder/decoder is also provided for `AnkiSettings`, since a `Deck`'s settings must be preserved between sessions.

@docs encoderSRSData, decoderSRSData, encoderAnkiSettings, decoderAnkiSettings


# Answering Cards

The SM-2 Anki algorithm depends on grading answers on a scale with one incorrect and three correct responses, except during the learning phase, in which case only 1 incorrect and two correct responses should be used. For some use cases, it may be able to programmatically determine the `Answer` of a user's response. In other cases, however, the user may need to self-report.

@docs Answer, answerCardInDeck, answerCard


# Due Cards

Besides answering cards, this package handles determining which cards in a `Deck` are due and require answering.

@docs getDueCardIndices, getDueCardIndicesWithDetails


# Card Details

If you require specific details for a single card, you may use the provided functionality here. If you need details for _all_ due cards, just use `getDueCardIndicesWithDetails`. You can also get all leeches using `getLeeches`.

@docs QueueDetails, getCardDetails, getLeeches

-}

import Array exposing (Array)
import Array.Extra
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import SpacedRepetition.Internal.SMTwoAnki exposing (Days(..), Ease, Lapses, Minutes(..), QueueStatus(..), Step, TimeInterval(..), createEase, createLapses, createStep, createTimeIntervalInDays, createTimeIntervalInMinutes, easeToFloat, lapsesToInt, minutesToDayInterval, stepToInt, timeIntervalToDays, timeIntervalToMinutes)
import Time
import Time.Extra exposing (Interval(..), diff)


{-| `AnkiSettings` customizes the behavior of this algorithm. Refer to [this section of the Anki manual](https://apps.ankiweb.net/docs/manual.html#deck-options) for more details. It may be created with the `createSettings` function and its values may be changed form default by piping to the following functions:


## New Cards

  - `setNewSteps : List Int -> AnkiSettings -> AnkiSettings` -- **Anki default: [1, 10]** Sets the interval steps (in minutes) that a card must go through while in the learning process. Answering `Again` or `Hard` will return a card to step 0. Answering `Easy` will instantly graduate a card. Answering `Good` will advance the card 1 step. If this list is empty, cards will instantly graduate the first time they are reviewed, regardless of the `Answer`. Note that an interval must be at least 1 minute long; any value less than 1 minute will result in an interval of 1 minute.

  - `setGraduatingInterval : Int -> AnkiSettings -> AnkiSettings` -- **Anki default: 1** Sets the initial interval (in days) a card will be scheduled for upon graduating from the learning phase. If `newSteps` is empty, all new cards will be scheduled for this interval when first encountered, regardless of answer. Note that this interval will not be fuzzed, per Anki's source. Values less than 1 will result in 1 being used.

  - `setEasyInterval : Int -> AnkiSettings -> AnkiSettings` -- **Anki default: 4** Sets the initial interval (in days) a card will be scheduled for upon instantly graduating from the learning phase with an answer of `Easy`. This value is not used if `newSteps` is empty. Note that this interval will not be fuzzed, per Anki's source. Values less than 1 will result in 1 being used.\`

  - `setStartingEase : Float -> AnkiSettings -> AnkiSettings` -- **Anki default: 2.5** Sets the initial ease that a card graduating from learning will have. Values less than 1.3 are ignored and result in 1.3 being used instead (as that is the minimum ease a card may have).


## Reviews

  - `setEasyBonus : Float -> AnkiSettings -> AnkiSettings` -- **Anki default 1.3** Sets the multiplier for the next interval of a card that was answered `Easy` (i.e. additional interval length over merely answering `Good`. Values less than 1.0 are ignored.

  - `setIntervalModifier : Float -> AnkiSettings -> AnkiSettings` -- **Anki default 1.0** Set the multiplier for **all** intervals, e.g. setting it to 0.8 will result in all review intervals being 80% as long as they would normally be. This multiplier may be used to increase/decrease retention at the cost of increasing/decreasing study time, respectively. Values less than or equal to zero are ignored.

  - `setMaximumInterval : Int -> AnkiSettings -> AnkiSettings` -- **Anki default: 36500** Set the upper limit on the time the algorithm will schedule a card for (in days). Small values may be used to cap time between seeing a card (to increase retention). Values less than 1 are ignored. Note that, due to the limitations of integers, an absolute maximum interval of 1491308 days (4085 years) exists. Since you almost certainly won't be alive in 4000 years to study a card, this shouldn't be a problem.

  - `setHardInterval : Float -> AnkiSettings -> AnkiSettings` -- **Anki default: 1.2** Set the multiplier applied to the previous interval when `Hard` is answered to a card. Note that intervals are forced to be at least 1 day longer than the previous one (before fuzzing). As such, values <= 1 will have no effect. Additionally, values of `hardInterval` that would result in a longer interval than that from answering `Good` (i.e. that are larger than the ease of the card) are ignored, with ease being used instead. As such, it is probably good practice to keep this value <= 1.3, as that is the lower limit on ease.


## Lapses

  - `lapseSteps : List Int -> AnkiSettings -> AnkiSettings` -- **Anki default: [10]** Set the interval steps (in minutes) that a card must go through while relearning after a lapse. Answering `Again` or `Hard` will return a card to step 0. Answering `Easy` will instantly graduate a card back to review. Answering `Good` will advance the card 1 step. If this list is empty, cards will instantly graduate the first time they are reviewed, regardless of the `Answer`.
  - `lapseNewInterval : Float -> AnkiSettings -> AnkiSettings` -- **Anki default: 0.0** Set a multiplier for the pre-lapse interval to determine the new interval after graduating a card back from re-learning. Values less than 0.0 are ignored Note that this interval is not fuzzed, per Anki's source.
  - `lapseMinimumInterval : Int -> AnkiSettings -> AnkiSettings` -- **Anki default: 1** Set a minimum bound on the previous option, with lapsed cards getting a new interval of at least this value. Answers less than 1 are ignored.
  - `leechThreshold : Int -> AnkiSettings -> AnkiSettings` -- **Anki default: 8** Set the number of lapses before a card is considered a "leech." `getDueCardIndices` will return the leech status of each card in the deck. Additionally, `getLeeches` will return all leeches in a deck (regardless of due status). Setting this value to less than or equal to 0 turns off leeches entirely.

-}
type alias AnkiSettings =
    { -- New Settings
      newSteps : List (TimeInterval Minutes)
    , graduatingInterval : TimeInterval Days
    , easyInterval : TimeInterval Days
    , startingEase : Float

    -- Review Settings
    , easyBonus : Float
    , intervalModifier : Float
    , maximumInterval : TimeInterval Days
    , hardInterval : Float

    -- Lapse Settings
    , lapseSteps : List (TimeInterval Minutes)
    , lapseNewInterval : Float
    , lapseMinimumInterval : TimeInterval Days
    , leechThreshold : Int
    }


{-| `createSettings` creates an `AnkiSettings` with the same default settings as the Anki program itself. It may be piped into the following functions to update from default:

  - `setNewSteps`
  - `setGraduatingInterval`
  - `setEasyInterval`
  - `setStartingEase`
  - `setEasyBonus`
  - `setIntervalModifier`
  - `setMaximumInterval`
  - `setHardInterval`
  - `setLapseSteps`
  - `setLapseNewInterval`
  - `setLapseMinimumInterval`
  - `setLeechThreshold`

An example follows:

    createSettings
        |> setNewSteps [ 1, 2, 3 ]
        |> setLapseSteps [ 4, 5, 6 ]
        |> setLeechThreshold 0

-}
createSettings : AnkiSettings
createSettings =
    -- Anki defaults
    { newSteps =
        [ createTimeIntervalInMinutes 1
        , createTimeIntervalInMinutes 10
        ]
    , graduatingInterval = createTimeIntervalInDays 1
    , easyInterval = createTimeIntervalInDays 4
    , startingEase = 2.5
    , easyBonus = 1.3
    , intervalModifier = 1.0
    , maximumInterval = createTimeIntervalInDays 36500
    , hardInterval = 1.2
    , lapseSteps = [ createTimeIntervalInMinutes 10 ]
    , lapseNewInterval = 0.0
    , lapseMinimumInterval = createTimeIntervalInDays 1
    , leechThreshold = 8
    }


{-| `setNewSteps` sets the the interval steps (in minutes) that a card must go through while in the learning process. Answering `Again` or `Hard` will return a card to step 0. Answering `Easy` will instantly graduate a card. Answering `Good` will advance the card 1 step. If this list is empty, cards will instantly graduate the first time they are reviewed, regardless of the `Answer`. Note that an interval must be at least 1 minute long; any value less than 1 minute will result in an interval of 1 minute.
-}
setNewSteps : List Int -> AnkiSettings -> AnkiSettings
setNewSteps l s =
    { s | newSteps = List.map createTimeIntervalInMinutes l }


{-| `setGraduatingInterval` sets the initial interval (in days) a card will be scheduled for upon graduating from the learning phase. If `newSteps` is empty, all new cards will be scheduled for this interval when first encountered, regardless of answer. Note that this interval will not be fuzzed, per Anki's source. Values less than 1 will result in 1 being used.
-}
setGraduatingInterval : Int -> AnkiSettings -> AnkiSettings
setGraduatingInterval i s =
    { s | graduatingInterval = createTimeIntervalInDays i }


{-| `setEasyInterval` sets the initial interval (in days) a card will be scheduled for upon instantly graduating from the learning phase with an answer of `Easy`. This value is not used if `newSteps` is empty. Note that this interval will not be fuzzed, per Anki's source. Values less than 1 will result in 1 being used.
-}
setEasyInterval : Int -> AnkiSettings -> AnkiSettings
setEasyInterval i s =
    { s | easyInterval = createTimeIntervalInDays i }


{-| `setStartingEase` sets the initial ease that a card graduating from learning will have. Values less than 1.3 are ignored and result in 1.3 being used instead (as that is the minimum ease a card may have).
-}
setStartingEase : Float -> AnkiSettings -> AnkiSettings
setStartingEase f s =
    { s | startingEase = f }


{-| `setEasyBonus` sets the multiplier for the next interval of a card that was answered `Easy` (i.e. additional interval length over merely answering `Good`. Values less than 1.0 are ignored.
-}
setEasyBonus : Float -> AnkiSettings -> AnkiSettings
setEasyBonus f s =
    { s | easyBonus = f }


{-| `setIntervalModifier` sets the multiplier for **all** intervals, e.g. setting it to 0.8 will result in all review intervals being 80% as long as they would normally be. This multiplier may be used to increase/decrease retention at the cost of increasing/decreasing study time, respectively. Values less than or equal to zero are ignored.
-}
setIntervalModifier : Float -> AnkiSettings -> AnkiSettings
setIntervalModifier f s =
    { s | intervalModifier = f }


{-| `setMaximumInterval` sets the upper limit on the time the algorithm will schedule a card for (in days). Small values may be used to cap time between seeing a card (to increase retention). Values less than 1 are ignored. Note that, due to the limitations of integers, an absolute maximum interval of 1491308 days (4085 years) exists. Since you almost certainly won't be alive in 4000 years to study a card, this shouldn't be a problem.
-}
setMaximumInterval : Int -> AnkiSettings -> AnkiSettings
setMaximumInterval i s =
    { s | maximumInterval = createTimeIntervalInDays i }


{-| `setHardInterval` sets the multiplier applied to the previous interval when `Hard` is answered to a card. Note that intervals are forced to be at least 1 day longer than the previous one. As such, values <= 1 will have no effect. Additionally, values of `hardInterval` that would result in a longer interval than that from answering `Good` (i.e. that are larger than the ease of the card) are ignored, with ease being used instead. As such, it is probably good practice to keep this value <= 1.3, as that is the lower limit on ease.
-}
setHardInterval : Float -> AnkiSettings -> AnkiSettings
setHardInterval f s =
    { s | hardInterval = f }


{-| `setLapseSteps` sets the interval steps (in minutes) that a card must go through while relearning after a lapse. Answering `Again` or `Hard` will return a card to step 0. Answering `Easy` will instantly graduate a card back to review. Answering `Good` will advance the card 1 step. If this list is empty, cards will instantly graduate the first time they are reviewed, regardless of the `Answer`.
-}
setLapseSteps : List Int -> AnkiSettings -> AnkiSettings
setLapseSteps l s =
    { s | lapseSteps = List.map createTimeIntervalInMinutes l }


{-| `setLapseNewInterval` sets a multiplier for the pre-lapse interval to determine the new interval after graduating a card back from re-learning. Values less than 0.0 are ignored Note that this interval is not fuzzed, per Anki's source.
-}
setLapseNewInterval : Float -> AnkiSettings -> AnkiSettings
setLapseNewInterval f s =
    { s | lapseNewInterval = f }


{-| `setLapseMinimumInterval` sets a minimum bound on the previous option, with lapsed cards getting a new interval of at least this value. Answers less than 1 are ignored.
-}
setLapseMinimumInterval : Int -> AnkiSettings -> AnkiSettings
setLapseMinimumInterval i s =
    { s | lapseMinimumInterval = createTimeIntervalInDays i }


{-| `setLeechThreshold` sets the number of lapses before a card is considered a "leech." `getDueCardIndices` will return the leech status of each card in the deck. Additionally, `getLeeches` will return all leeches in a deck (regardless of due status). Setting this value to less than or equal to 0 turns off leeches entirely.
-}
setLeechThreshold : Int -> AnkiSettings -> AnkiSettings
setLeechThreshold i s =
    { s | leechThreshold = i }


{-| A `Card` represents a single question or unit of knowledge the user will review. In general terms, each would represent a single flashcard. `Card` is defined as an extensible record; as such, whatever necessary custom fields for a use case may simply be included in the record, e.g.:

    type alias MyFlashcard =
        Card { prompt : String, answer : String }

A `Card` contains only the information necessary for scheduling and nothing else; all other information should be added as in the above example.

-}
type alias Card a =
    { a
        | srsData : SRSData
    }


{-| A `Deck` represents a list of cards to be studied (this might be called a "collection" in other software). It is a record with field `cards`, an `Array` of `Card` and field `settings` of `AnkiSettings`. Maintaining the state of a `Deck` may be handled by the user of the module or by this module itself. In general, it is probably best not to add a massive quantity of new (unstudied) cards to a deck at once.
-}
type alias Deck a b =
    { a
        | cards : Array (Card b)
        , settings : AnkiSettings
    }


{-| `SRSData` contains all data necessary for the Anki system and may be created with the `newSRSData` function. It may additionally be saved/loaded using the Json encoder/decoder in this package
-}
type alias SRSData =
    QueueStatus


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

        Learning step lastReviewed ->
            Encode.object
                [ ( "step", stepEncoder step )
                , ( "lastReviewed", Encode.int <| Time.posixToMillis lastReviewed // 1000 ) -- Encode time in seconds; loss of precision is acceptable
                ]

        Lapsed ease step interval lastReviewed lapses ->
            Encode.object
                [ ( "step", stepEncoder step )
                , ( "oldInterval", intervalEncoder interval )
                , ( "lastReviewed", Encode.int <| Time.posixToMillis lastReviewed // 1000 ) -- Encode time in seconds; loss of precision is acceptable
                , ( "ease", easeEncoder ease )
                , ( "lapses", lapsesEncoder lapses )
                ]

        Review ease interval lastReviewed lapses ->
            Encode.object
                [ ( "interval", intervalEncoder interval )
                , ( "lastReviewed", Encode.int <| Time.posixToMillis lastReviewed // 1000 ) -- Encode time in seconds; loss of precision is acceptable
                , ( "ease", easeEncoder ease )
                , ( "lapses", lapsesEncoder lapses )
                ]


{-| `decoderSRSData` provides a Json decoder for decoding `SRSData` for a `Card`.
-}
decoderSRSData : Decode.Decoder SRSData
decoderSRSData =
    Decode.oneOf
        [ Decode.null New
        , Decode.map5 Lapsed
            (Decode.map createEase <| Decode.field "ease" Decode.float)
            (Decode.map createStep <| Decode.field "step" Decode.int)
            (Decode.map TimeInterval <| Decode.field "oldInterval" Decode.int)
            (Decode.map (\t -> Time.millisToPosix <| t * 1000) <| Decode.field "lastReviewed" Decode.int)
            (Decode.map createLapses <| Decode.field "lapses" Decode.int)
        , Decode.map2 Learning
            (Decode.map createStep <| Decode.field "step" Decode.int)
            (Decode.map (\t -> Time.millisToPosix <| t * 1000) <| Decode.field "lastReviewed" Decode.int)
        , Decode.map4 Review
            (Decode.map createEase <| Decode.field "ease" Decode.float)
            (Decode.map TimeInterval <| Decode.field "interval" Decode.int)
            (Decode.map (\t -> Time.millisToPosix <| t * 1000) <| Decode.field "lastReviewed" Decode.int)
            (Decode.map createLapses <| Decode.field "lapses" Decode.int)
        ]


{-| `encoderAnkiSettings` provides a Json encoder for encoding `AnkiSettings` from a `Deck`.
-}
encoderAnkiSettings : AnkiSettings -> Encode.Value
encoderAnkiSettings settings =
    Encode.object
        [ ( "newSteps", Encode.list Encode.int <| List.map timeIntervalToMinutes settings.newSteps )
        , ( "graduatingInterval", Encode.int <| timeIntervalToDays settings.graduatingInterval )
        , ( "easyInterval", Encode.int <| timeIntervalToDays settings.easyInterval )
        , ( "startingEase", Encode.float settings.startingEase )
        , ( "easyBonus", Encode.float settings.easyBonus )
        , ( "intervalModifier", Encode.float settings.intervalModifier )
        , ( "maximumInterval", Encode.int <| timeIntervalToDays settings.maximumInterval )
        , ( "hardInterval", Encode.float settings.hardInterval )
        , ( "lapseSteps", Encode.list Encode.int <| List.map timeIntervalToMinutes settings.lapseSteps )
        , ( "lapseNewInterval", Encode.float settings.lapseNewInterval )
        , ( "lapseMinimumInterval", Encode.int <| timeIntervalToDays settings.lapseMinimumInterval )
        , ( "leechThreshold", Encode.int settings.leechThreshold )
        ]


{-| `decoderAnkiSettings` provides a Json decoder for decoding `AnkiSettings` for a `Deck`.
-}
decoderAnkiSettings : Decode.Decoder AnkiSettings
decoderAnkiSettings =
    let
        with : (a -> b -> b) -> String -> Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder b
        with f field decoder =
            Decode.map2 (<|) (Decode.map f <| Decode.field field decoder)
    in
    Decode.succeed createSettings
        |> with setNewSteps "newSteps" (Decode.list Decode.int)
        |> with setGraduatingInterval "graduatingInterval" Decode.int
        |> with setEasyInterval "easyInterval" Decode.int
        |> with setStartingEase "startingEase" Decode.float
        |> with setEasyBonus "easyBonus" Decode.float
        |> with setIntervalModifier "intervalModifier" Decode.float
        |> with setMaximumInterval "maximumInterval" Decode.int
        |> with setHardInterval "hardInterval" Decode.float
        |> with setLapseSteps "lapseSteps" (Decode.list Decode.int)
        |> with setLapseNewInterval "lapseNewInterval" Decode.float
        |> with setLapseMinimumInterval "lapseMinimumInterval" Decode.int
        |> with setLeechThreshold "leechThreshold" Decode.int


{-| The `Answer` type represents how accurate/certain a user's response was to a card and must be passed to `answerCard` whenever a `Card` is reviewed. This package uses the same names as Anki, as presented below:

  - `Again` -- An incorrect response.

  - `Hard` -- A correct response that was challenging to produce. It is not necessary to present this as an option for answering cards in the Learning or Lapsed queues, as it has the same effect as `Again` in those cases, namely resetting the card to the start of the queue.

  - `Good` -- A correct response of appropriate difficulty.

  - `Easy` -- A correct response that was excessively easy to produce (will increase ease and interval faster)

-}
type Answer
    = Again
    | Hard -- Synonym for Again when learning
    | Good
    | Easy


{-| `answerCardInDeck` functions analogously to `answerCard` but handles maintenance of the `Deck`, which is typically what one would desire. When a card is presented to the user and answered, `answerCardInDeck` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module), an `Answer`, the index of the card in the `Deck`, and the `Deck` itself. It returns the updated `Deck`. Use this function if you simply want to store a `Deck` and not worry about updating it manually (which is most likely what you want). Otherwise, use `answerCard` to handle updating the `Deck` manually. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases. Note that if an invalid (out of bounds) index is passed, the `Deck` is returned unaltered.
-}
answerCardInDeck : Time.Posix -> Answer -> Int -> Deck a b -> Deck a b
answerCardInDeck time answer i deck =
    { deck
        | cards =
            Array.Extra.update i (answerCard time answer deck.settings) deck.cards
    }


{-| When a card is presented to the user and answered, `answerCard` should be called with the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and an `Answer`. It returns the updated card, which should replace the card in the `Deck`. Use this function if you want to handle updating the `Deck` manually; otherwise, use `answerCardInDeck`. Handling the presentation of a card is the responsibility of the implementing program, as various behaviors might be desirable in different cases.
-}
answerCard : Time.Posix -> Answer -> AnkiSettings -> Card a -> Card a
answerCard time answer settings card =
    updateEase answer card
        |> scheduleCard settings time answer


{-| `getDueCardIndices` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the indices of the subset of the `Deck` that is due for review. The returned indices will be sorted in the following order:

1.  Lapsed cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Review cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
3.  Learning cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
4.  Any new cards in the deck (never having been studied before).

`getDueCardIndices` will show cards up to 20 minutes early, as per Anki.

-}
getDueCardIndices : Time.Posix -> Deck a b -> List Int
getDueCardIndices time deck =
    Array.toIndexedList deck.cards
        |> List.filter
            (isDue deck.settings time << Tuple.second)
        |> List.sortWith
            (\c1 c2 -> sortDue deck.settings time (Tuple.second c1) (Tuple.second c2))
        |> List.Extra.reverseMap Tuple.first


{-| `getDueCardIndicesWithDetails` takes the current time (in the `Time.Posix` format returned by the `now` task of the core `Time` module) and a `Deck` and returns the subset of the `Deck` that is due for review as a list of records, providing their index, which queue they are currently in (e.g. whether they are being learned or reviewed) along with any relevant queue details, and whether or not they are leeches. The returned indices will be sorted in the following order:

1.  Lapsed cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
2.  Review cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
3.  Learning cards overdue for review
    1.  Cards more overdue (by proportion of interval)
    2.  Cards less overdue (by proportion of interval)
4.  Any new cards in the deck (never having been studied before).

`getDueCardIndicesWithDetails` will show cards up to 20 minutes early, as per Anki.

-}
getDueCardIndicesWithDetails :
    Time.Posix
    -> Deck a b
    -> List { index : Int, queueDetails : QueueDetails, isLeech : Bool }
getDueCardIndicesWithDetails time deck =
    Array.toIndexedList deck.cards
        |> List.filter
            (isDue deck.settings time << Tuple.second)
        |> List.sortWith
            (\c1 c2 -> sortDue deck.settings time (Tuple.second c1) (Tuple.second c2))
        |> List.Extra.reverseMap
            (\( index, card ) ->
                { index = index
                , queueDetails = getQueueDetails deck.settings card
                , isLeech = isLeech deck.settings card
                }
            )


{-| `getLeeches` takes a `Deck` and returns the indices of the subset of the `Deck` that are leeches (as `List Int`). The returned indices will be sorted in the following order:

1.  Cards with more lapses
2.  Cards with fewer lapses

-}
getLeeches : Deck a b -> List Int
getLeeches deck =
    Array.toIndexedList deck.cards
        |> List.filter (isLeech deck.settings << Tuple.second)
        |> List.sortWith
            (\( _, c1 ) ( _, c2 ) -> compare (numberOfLapses c1) (numberOfLapses c2))
        |> List.Extra.reverseMap Tuple.first


{-| `QueueDetails` represents the current status of a card.

  - `NewCard` -- A card that has never before been studied (encountered) by the user.

  - `LearningQueue {...}` -- A card that is in the initial learning queue, progressing through the steps specified in `AnkiSettings.newSteps`.
      - `lastSeen : Time.Posix` -- The date and time the card was last reviewed.
      - `intervalInMinutes : Int` -- The interval, in minutes from the date last seen, that the card is slated for review in.

  - `ReviewQueue {...}` -- A card that is being reviewed for retention.
      - `lastSeen : Time.Posix` -- The date and time the card was last reviewed.
      - `intervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in.
      - `lapses : Int` -- The number of times the card has "lapsed," i.e. been forgotten/incorrectly answered by the user.

  - `LapsedQueue {...}` -- A card that has lapsed, i.e. one that was being reviewed but was answered incorrectly and is now being re-learned.
      - `lastSeen : Time.Posix` -- The date and time the card was last reviewed.
      - `reviewIntervalInDays : Int` -- The interval, in days from the date last seen, that the card was slated for review in prior to last being forgotten/ answered incorrectly.
      - `intervalInMinutes : Int` -- The interval, in minutes from the date last seen, that the card is slated for review in.
      - `lapses : Int` -- The number of times the card has "lapsed," i.e. been forgotten/incorrectly answered by the user.

-}
type QueueDetails
    = NewCard
    | LearningQueue
        { lastSeen : Time.Posix
        , intervalInMinutes : Int
        }
    | ReviewQueue
        { lastSeen : Time.Posix
        , intervalInDays : Int
        , lapses : Int
        }
    | LapsedQueue
        { lastSeen : Time.Posix
        , formerIntervalInDays : Int
        , intervalInMinutes : Int
        , lapses : Int
        }


{-| `getCardDetails` returns the current queue status for a given card and whether or not it is a leech. If you require this for every due card, simply use `getDueCardIndicesWithDetails`.
-}
getCardDetails : AnkiSettings -> Card a -> { queueDetails : QueueDetails, isLeech : Bool }
getCardDetails s c =
    { queueDetails = getQueueDetails s c, isLeech = isLeech s c }



-- * Non-exposed only below here


numberOfLapses : Card a -> Int
numberOfLapses card =
    case card.srsData of
        Review _ _ _ lapses ->
            lapsesToInt lapses

        Lapsed _ _ _ _ lapses ->
            lapsesToInt lapses

        _ ->
            0


isLeech : AnkiSettings -> Card a -> Bool
isLeech settings card =
    if settings.leechThreshold <= 0 then
        False

    else
        case card.srsData of
            Review _ _ _ lapses ->
                lapsesToInt lapses >= settings.leechThreshold

            Lapsed _ _ _ _ lapses ->
                lapsesToInt lapses >= settings.leechThreshold

            _ ->
                False


getQueueDetails : AnkiSettings -> Card a -> QueueDetails
getQueueDetails s c =
    case c.srsData of
        New ->
            NewCard

        Learning _ lastReviewed ->
            LearningQueue
                { lastSeen = lastReviewed
                , intervalInMinutes = getCurrentIntervalInMinutes s c
                }

        Review _ interval lastReviewed lapses ->
            ReviewQueue
                { lastSeen = lastReviewed
                , intervalInDays = timeIntervalToDays interval
                , lapses = lapsesToInt lapses
                }

        Lapsed _ _ lastInterval lastReviewed lapses ->
            LapsedQueue
                { lastSeen = lastReviewed
                , formerIntervalInDays = timeIntervalToDays lastInterval
                , intervalInMinutes = getCurrentIntervalInMinutes s c
                , lapses = lapsesToInt lapses
                }


sortDue : AnkiSettings -> Time.Posix -> Card a -> Card a -> Order
sortDue settings time c1 c2 =
    case ( c1.srsData, c2.srsData ) of
        ( New, New ) ->
            EQ

        ( New, _ ) ->
            LT

        ( _, New ) ->
            GT

        ( Learning _ _, Learning _ _ ) ->
            let
                ( overdueAmt1, _ ) =
                    overdueAmount settings time c1

                ( overdueAmt2, _ ) =
                    overdueAmount settings time c2
            in
            if overdueAmt1 >= overdueAmt2 then
                GT

            else
                LT

        ( Learning _ _, _ ) ->
            LT

        ( _, Learning _ _ ) ->
            GT

        ( Review _ _ _ _, Review _ _ _ _ ) ->
            let
                ( overdueAmt1, _ ) =
                    overdueAmount settings time c1

                ( overdueAmt2, _ ) =
                    overdueAmount settings time c2
            in
            if overdueAmt1 >= overdueAmt2 then
                GT

            else
                LT

        ( Review _ _ _ _, _ ) ->
            LT

        ( _, Review _ _ _ _ ) ->
            GT

        ( Lapsed _ _ _ _ _, Lapsed _ _ _ _ _ ) ->
            let
                ( overdueAmt1, _ ) =
                    overdueAmount settings time c1

                ( overdueAmt2, _ ) =
                    overdueAmount settings time c2
            in
            if overdueAmt1 >= overdueAmt2 then
                GT

            else
                LT


isDue : AnkiSettings -> Time.Posix -> Card a -> Bool
isDue settings time card =
    case card.srsData of
        New ->
            True

        _ ->
            let
                ( _, minutesOverdue ) =
                    overdueAmount settings time card
            in
            minutesOverdue >= -20


intervalToScale : AnkiSettings -> Answer -> Time.Posix -> Card a -> Int
intervalToScale settings answer time card =
    let
        interval =
            getCurrentIntervalInMinutes settings card
    in
    case answer of
        Again ->
            0

        Hard ->
            interval

        Good ->
            overdueAmount settings time card
                |> Tuple.second
                |> (\minutesOverdue -> minutesOverdue // 2)
                |> (+) interval
                |> max interval

        Easy ->
            overdueAmount settings time card
                |> Tuple.second
                |> (+) interval
                |> max interval


overdueAmount : AnkiSettings -> Time.Posix -> Card a -> ( Float, Int )
overdueAmount settings time card =
    let
        interval =
            getCurrentIntervalInMinutes settings card

        reviewed : Time.Posix
        reviewed =
            case card.srsData of
                New ->
                    Time.millisToPosix 0

                Learning _ t ->
                    t

                Review _ _ t _ ->
                    t

                Lapsed _ _ _ t _ ->
                    t

        minuteDiff : Int
        minuteDiff =
            diff Minute Time.utc reviewed time

        minutesOverdue : Int
        minutesOverdue =
            minuteDiff - interval
    in
    case card.srsData of
        New ->
            ( 1.0, 0 )

        _ ->
            -- (Relative Amount Overdue, Absolute Minutes Overdue)
            ( toFloat minutesOverdue / toFloat interval, minutesOverdue )


scheduleCard : AnkiSettings -> Time.Posix -> Answer -> Card a -> Card a
scheduleCard settings time answer card =
    let
        newReviewInterval =
            intervalToScale settings answer time card

        boundInterval : TimeInterval Days -> TimeInterval Days
        boundInterval i =
            createTimeIntervalInDays
                (min
                    (timeIntervalToDays settings.maximumInterval)
                    (timeIntervalToDays i)
                )

        createReview : Ease -> TimeInterval Days -> Int -> SRSData
        createReview ease interval lapses =
            Review
                ease
                (boundInterval interval)
                time
                (createLapses lapses)

        scaleReviewInterval : Float -> Int -> TimeInterval Days
        scaleReviewInterval f oldInterval =
            -- Increase interval by a factor f and ensure it's at least 1 day longer
            scaleIntervalWithMinimum (f * settings.intervalModifier) (oldInterval + 1440) oldInterval

        scaleIntervalWithMinimum : Float -> Int -> Int -> TimeInterval Days
        scaleIntervalWithMinimum f minInterval oldInterval =
            -- This magic number is max int; truncate does not play well with larger values.
            toFloat oldInterval
                |> (*) f
                |> min 2147483647
                |> truncate
                |> max minInterval
                |> minutesToDayInterval

        moveToReview =
            createReview
                (createEase settings.startingEase)
                settings.graduatingInterval
                0

        advanceStep =
            case card.srsData of
                New ->
                    if not <| List.isEmpty settings.newSteps then
                        Learning (createStep 0) time

                    else
                        moveToReview

                Learning currentStep _ ->
                    if stepToInt currentStep + 1 >= List.length settings.newSteps then
                        moveToReview

                    else
                        Learning (createStep (stepToInt currentStep + 1)) time

                Lapsed ease currentStep oldInterval _ lapses ->
                    if stepToInt currentStep + 1 >= List.length settings.lapseSteps then
                        createReview
                            ease
                            (scaleIntervalWithMinimum
                                settings.lapseNewInterval
                                (timeIntervalToMinutes settings.lapseMinimumInterval)
                                (timeIntervalToMinutes oldInterval)
                            )
                            (lapsesToInt lapses)

                    else
                        Lapsed ease (createStep <| stepToInt currentStep + 1) oldInterval time lapses

                _ ->
                    -- This case should never happen and will be caught by tests
                    New

        newStatus =
            case card.srsData of
                New ->
                    advanceStep

                Learning _ _ ->
                    case answer of
                        Easy ->
                            -- Instantly graduate if answer was Easy
                            createReview
                                (createEase settings.startingEase)
                                settings.easyInterval
                                0

                        Good ->
                            advanceStep

                        _ ->
                            -- Go back to beginning of learning
                            Learning (createStep 0) time

                Lapsed ease _ oldInterval _ lapses ->
                    if not <| List.isEmpty settings.lapseSteps then
                        case answer of
                            Easy ->
                                -- Instantly graduate if answer was Easy
                                createReview
                                    ease
                                    (scaleIntervalWithMinimum
                                        settings.lapseNewInterval
                                        (timeIntervalToMinutes settings.lapseMinimumInterval)
                                        (timeIntervalToMinutes oldInterval)
                                    )
                                    (lapsesToInt lapses)

                            Good ->
                                advanceStep

                            _ ->
                                -- Go back to beginning of lapses
                                Lapsed ease (createStep 0) oldInterval time lapses

                    else
                        -- Instantly graduate back to review if no lapse steps
                        createReview
                            ease
                            (scaleIntervalWithMinimum
                                settings.lapseNewInterval
                                (timeIntervalToMinutes settings.lapseMinimumInterval)
                                (timeIntervalToMinutes oldInterval)
                            )
                            (lapsesToInt lapses)

                Review ease lastInterval _ lapses ->
                    case answer of
                        -- Force next interval to be at least 1 day longer except for lapse
                        Easy ->
                            createReview
                                ease
                                (fuzzInterval time
                                    << scaleReviewInterval (easeToFloat ease * max 1 settings.easyBonus)
                                 <|
                                    newReviewInterval
                                )
                                (lapsesToInt lapses)

                        Good ->
                            createReview
                                ease
                                (fuzzInterval time
                                    << scaleReviewInterval (easeToFloat ease)
                                 <|
                                    newReviewInterval
                                )
                                (lapsesToInt lapses)

                        Hard ->
                            createReview
                                ease
                                (fuzzInterval time
                                    << scaleReviewInterval (min (easeToFloat ease) settings.hardInterval)
                                 <|
                                    newReviewInterval
                                )
                                (lapsesToInt lapses)

                        Again ->
                            Lapsed ease (createStep 0) lastInterval time (createLapses <| lapsesToInt lapses + 1)
    in
    { card | srsData = newStatus }


easeEncoder : Ease -> Encode.Value
easeEncoder ease =
    Encode.float <| easeToFloat ease


stepEncoder : Step -> Encode.Value
stepEncoder step =
    Encode.int <| stepToInt step


lapsesEncoder : Lapses -> Encode.Value
lapsesEncoder lapses =
    Encode.int <| lapsesToInt lapses


intervalEncoder : TimeInterval Days -> Encode.Value
intervalEncoder interval =
    case interval of
        TimeInterval i ->
            Encode.int i


updateEase : Answer -> Card a -> Card a
updateEase answer card =
    case card.srsData of
        Review oldEase interval reviewed lapses ->
            let
                newEase =
                    case answer of
                        Again ->
                            createEase <| easeToFloat oldEase - 0.2

                        Hard ->
                            createEase <| easeToFloat oldEase - 0.15

                        Good ->
                            oldEase

                        Easy ->
                            createEase <| easeToFloat oldEase + 0.15
            in
            { card | srsData = Review newEase interval reviewed lapses }

        _ ->
            -- Ease ONLY gets updated when in Review phase
            card


getCurrentIntervalInMinutes : AnkiSettings -> Card a -> Int
getCurrentIntervalInMinutes settings { srsData } =
    case srsData of
        New ->
            0

        Learning step _ ->
            List.Extra.getAt (stepToInt step) settings.newSteps
                |> Maybe.map timeIntervalToMinutes
                |> Maybe.withDefault 1

        Lapsed _ step _ _ _ ->
            List.Extra.getAt (stepToInt step) settings.lapseSteps
                |> Maybe.map timeIntervalToMinutes
                |> Maybe.withDefault 1

        Review _ interval _ _ ->
            timeIntervalToMinutes interval


fuzzInterval : Time.Posix -> TimeInterval Days -> TimeInterval Days
fuzzInterval time interval =
    Random.step (fuzzedIntervalGenerator interval) (Random.initialSeed <| Time.posixToMillis time)
        |> Tuple.first


fuzzedIntervalGenerator : TimeInterval Days -> Random.Generator (TimeInterval Days)
fuzzedIntervalGenerator timeInterval =
    -- These are the fuzz amounts per Anki's source
    let
        interval =
            timeIntervalToDays timeInterval

        fuzz : Int
        fuzz =
            if interval < 7 then
                round << max 1 <| toFloat interval * 0.25

            else if interval < 30 then
                round << max 2 <| toFloat interval * 0.15

            else
                round << max 4 <| toFloat interval * 0.05

        ( minInterval, maxInterval ) =
            if interval < 2 then
                ( 1, 1 )

            else if interval == 2 then
                ( 2, 3 )

            else
                ( interval - fuzz, interval + fuzz )
    in
    Random.map createTimeIntervalInDays <| Random.int minInterval maxInterval
