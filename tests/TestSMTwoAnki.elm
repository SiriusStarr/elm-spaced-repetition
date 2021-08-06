module TestSMTwoAnki exposing
    ( cardSchedulingTests
    , suiteAnswerCard
    , suiteAnswerCardInDeck
    , suiteGetDue
    , suiteGetDueWithDetails
    , suiteGetLeeches
    , suiteJson
    )

import Array exposing (Array)
import Array.Extra as ArrayX
import Basics.Extra exposing (flip)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
    exposing
        ( Fuzzer
        , floatRange
        , int
        , intRange
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListX
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import SpacedRepetition.Internal.SMTwoAnki
    exposing
        ( Days
        , Ease
        , Minutes
        , QueueStatus(..)
        , TimeInterval
        , createEase
        , easeToFloat
        , minutesToDayInterval
        , timeIntervalFromDays
        , timeIntervalFromMinutes
        , timeIntervalToDays
        , timeIntervalToMinutes
        )
import SpacedRepetition.SMTwoAnki
    exposing
        ( AnkiSettings
        , Answer(..)
        , Card
        , QueueDetails(..)
        , SRSData
        , answerCard
        , answerCardInDeck
        , decoderAnkiSettings
        , decoderSRSData
        , encoderAnkiSettings
        , encoderSRSData
        , getDueCardIndices
        , getDueCardIndicesWithDetails
        , getLeeches
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)
import Util exposing (boundedLessThan, fuzzNatural, fuzzTime)


{-| Fuzz an `Ease`.
-}
fuzzEase : Fuzzer Ease
fuzzEase =
    Fuzz.map createEase (floatRange 0 1000)


{-| Fuzz an interval in minutes.
-}
fuzzMinuteInterval : Fuzzer (TimeInterval Minutes)
fuzzMinuteInterval =
    Fuzz.map timeIntervalFromMinutes <| intRange 0 52560000


{-| Fuzz an interval in days.
-}
fuzzDayInterval : Fuzzer (TimeInterval Days)
fuzzDayInterval =
    Fuzz.map timeIntervalFromDays <| intRange 0 36500


{-| Fuzz a review history for a card.
-}
fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    Fuzz.oneOf
        [ Fuzz.constant New
        , Fuzz.map Learning fuzzLearning
        , Fuzz.map Review fuzzReview
        , Fuzz.map Lapsed fuzzLapsed
        ]


{-| The data associated with a card in the Learning queue.
-}
type alias LearningData =
    { lastReviewed : Time.Posix, step : Natural }


{-| Fuzz data for a card in the Learning queue.
-}
fuzzLearning : Fuzzer LearningData
fuzzLearning =
    Fuzz.map2
        (\lastReviewed step ->
            { lastReviewed = lastReviewed
            , step = step
            }
        )
        fuzzTime
        fuzzNatural


{-| The data associated with a card in the Lapsed queue.
-}
type alias LapsedData =
    { ease : Ease
    , lapses : Natural
    , lastReviewed : Time.Posix
    , oldInterval : TimeInterval Days
    , step : Natural
    }


{-| Fuzz data for a card in the Lapsed queue.
-}
fuzzLapsed : Fuzzer LapsedData
fuzzLapsed =
    Fuzz.map5
        (\ease lapses lastReviewed oldInterval step ->
            { ease = ease
            , lapses = lapses
            , lastReviewed = lastReviewed
            , oldInterval = oldInterval
            , step = step
            }
        )
        fuzzEase
        fuzzNatural
        fuzzTime
        fuzzDayInterval
        fuzzNatural


{-| The data associated with a card in the Review queue.
-}
type alias ReviewData =
    { ease : Ease
    , interval : TimeInterval Days
    , lapses : Natural
    , lastReviewed : Time.Posix
    }


{-| Fuzz data for a card in the Review queue.
-}
fuzzReview : Fuzzer ReviewData
fuzzReview =
    Fuzz.map4
        (\ease interval lapses lastReviewed ->
            { ease = ease
            , interval = interval
            , lapses = lapses
            , lastReviewed = lastReviewed
            }
        )
        fuzzEase
        fuzzDayInterval
        fuzzNatural
        fuzzTime


{-| Fuzz a card.
-}
fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    Fuzz.map (\d -> { srsData = d }) fuzzSRSData


{-| Fuzz a deck of cards and settings.
-}
fuzzDeck : Fuzzer { cards : Array { srsData : SRSData }, settings : AnkiSettings }
fuzzDeck =
    Fuzz.map2 (\c s -> { cards = c, settings = s }) (Fuzz.array fuzzCard) fuzzSettings


{-| Fuzz settings for the Anki algorithm.
-}
fuzzSettings : Fuzzer AnkiSettings
fuzzSettings =
    Fuzz.map
        (\newSteps graduatingInterval easyInterval startingEase easyBonus intervalModifier maximumInterval hardInterval lapseSteps lapseNewInterval lapseMinimumInterval leechThreshold ->
            { easyBonus = easyBonus
            , easyInterval = easyInterval
            , graduatingInterval = graduatingInterval
            , hardInterval = hardInterval
            , intervalModifier = intervalModifier
            , lapseMinimumInterval = lapseMinimumInterval
            , lapseNewInterval = lapseNewInterval
            , lapseSteps = lapseSteps
            , leechThreshold = leechThreshold
            , maximumInterval = maximumInterval
            , newSteps = newSteps
            , startingEase = startingEase
            }
        )
        (Fuzz.list fuzzMinuteInterval)
        |> Fuzz.andMap fuzzDayInterval
        |> Fuzz.andMap fuzzDayInterval
        |> Fuzz.andMap fuzzEase
        |> Fuzz.andMap (floatRange 0 1000)
        |> Fuzz.andMap (floatRange 0 1000)
        |> Fuzz.andMap fuzzDayInterval
        |> Fuzz.andMap (floatRange 0 1000)
        |> Fuzz.andMap (Fuzz.list fuzzMinuteInterval)
        |> Fuzz.andMap (floatRange 0 1000)
        |> Fuzz.andMap fuzzDayInterval
        |> Fuzz.andMap fuzzNatural


{-| Fuzz two cards that are identical other than having been last reviewed at
different times.
-}
fuzzDiffOverdueCards : Fuzzer ( ReviewData, ReviewData )
fuzzDiffOverdueCards =
    Fuzz.map5
        (\ease interval t1 t2 lapses ->
            ( { ease = ease
              , interval = interval
              , lapses = lapses
              , lastReviewed = t1
              }
            , { ease = ease
              , interval = interval
              , lapses = lapses
              , lastReviewed = t2
              }
            )
        )
        fuzzEase
        fuzzDayInterval
        fuzzTime
        fuzzTime
        fuzzNatural


{-| Fuzz an answer quality.
-}
fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    Fuzz.oneOf
        [ Fuzz.constant Again
        , Fuzz.constant Hard
        , Fuzz.constant Good
        , Fuzz.constant Easy
        ]


{-| Fuzz a complete response to a card, with time, answer quality, and settings.
-}
fuzzResponse : Fuzzer ( Time.Posix, Answer, AnkiSettings )
fuzzResponse =
    Fuzz.map3 (\t a s -> ( t, a, s )) fuzzTime fuzzAnswer fuzzSettings


{-| Fuzz a card with an extra field.
-}
fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    Fuzz.map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


{-| Get the ease of a card as a float.
-}
easeFloatFromCard : { srsData : SRSData } -> Maybe Float
easeFloatFromCard c =
    case c.srsData of
        Lapsed { ease } ->
            Just <| easeToFloat ease

        Review { ease } ->
            Just <| easeToFloat ease

        _ ->
            Nothing


{-| Get the date last reviewed from a card.
-}
lastReviewedFromCard : Card a -> Maybe Time.Posix
lastReviewedFromCard c =
    case c.srsData of
        Lapsed { lastReviewed } ->
            Just lastReviewed

        Learning { lastReviewed } ->
            Just lastReviewed

        New ->
            Nothing

        Review { lastReviewed } ->
            Just lastReviewed


{-| Get the review interval from a card as a number of minutes, only for cards
in the Review queue.
-}
reviewIntervalFromCard : Card a -> Maybe Int
reviewIntervalFromCard { srsData } =
    case srsData of
        Review { interval } ->
            Just <| timeIntervalToMinutes interval

        _ ->
            Nothing


{-| Get the current (or pre-lapse) interval in minutes from a card.
-}
dayIntervalFromCard : Card a -> Maybe Int
dayIntervalFromCard { srsData } =
    case srsData of
        Lapsed { oldInterval } ->
            Just <| timeIntervalToMinutes oldInterval

        Review { interval } ->
            Just <| timeIntervalToMinutes interval

        _ ->
            Nothing


{-| Get the review interval from a card as a number of minutes.
-}
getInterval : AnkiSettings -> SRSData -> Int
getInterval settings srsData =
    case srsData of
        Lapsed { step } ->
            ListX.getAt (Natural.toInt step) settings.lapseSteps
                |> Maybe.map timeIntervalToMinutes
                |> Maybe.withDefault 1

        Learning { step } ->
            ListX.getAt (Natural.toInt step) settings.newSteps
                |> Maybe.map timeIntervalToMinutes
                |> Maybe.withDefault 1

        New ->
            0

        Review { interval } ->
            timeIntervalToMinutes interval


{-| Determine how many minutes overdue a card is.
-}
overdueAmount : AnkiSettings -> Time.Posix -> SRSData -> Int
overdueAmount settings time srsData =
    case srsData of
        New ->
            0

        _ ->
            let
                reviewed : Time.Posix
                reviewed =
                    lastReviewedFromCard { srsData = srsData }
                        |> Maybe.withDefault (Time.millisToPosix 0)
            in
            diff Minute Time.utc reviewed time - getInterval settings srsData


{-| Given an answer quality, determine what the next interval for a card should
be, in minutes.
-}
nextInterval : AnkiSettings -> Answer -> Time.Posix -> ReviewData -> Int
nextInterval settings answer time old =
    let
        oldEase : Float
        oldEase =
            easeToFloat old.ease

        newEase : Float
        newEase =
            case answer of
                Again ->
                    0

                Easy ->
                    oldEase + 0.15

                Good ->
                    oldEase

                Hard ->
                    max 1.3 <| oldEase - 0.15

        oldIntervalInMinutes : Int
        oldIntervalInMinutes =
            timeIntervalToMinutes old.interval

        modifier : Float
        modifier =
            case answer of
                Again ->
                    0

                Easy ->
                    newEase * max 1 settings.easyBonus

                Good ->
                    newEase

                Hard ->
                    min newEase settings.hardInterval

        scaleInterval : Int
        scaleInterval =
            case answer of
                Again ->
                    0

                Easy ->
                    overdueAmount settings time (Review old)
                        |> (+) oldIntervalInMinutes
                        |> max oldIntervalInMinutes

                Good ->
                    overdueAmount settings time (Review old)
                        |> (\minutesOverdue -> minutesOverdue // 2)
                        |> (+) oldIntervalInMinutes
                        |> max oldIntervalInMinutes

                Hard ->
                    oldIntervalInMinutes
    in
    modifier
        * settings.intervalModifier
        * toFloat scaleInterval
        -- This magic number is max int; truncate does not play well with larger values.
        |> min 2147483647
        |> truncate
        |> max (scaleInterval + 1440)


{-| Given an answer and an old and new review history, expect it to have been
updated appropriately.
-}
expectReviewUpdate :
    AnkiSettings
    -> Answer
    -> Time.Posix
    -> ReviewData
    -> SRSData
    -> Expectation
expectReviewUpdate settings answer time oldData newData =
    let
        ( oldEase, oldLapses ) =
            ( oldData.ease, oldData.lapses )
    in
    case ( newData, answer ) of
        ( Review _, Again ) ->
            Expect.fail "Card should have lapsed"

        ( Review r, Easy ) ->
            Expect.all
                [ \{ ease } -> Expect.within (Absolute 1.0e-9) (0.15 + easeToFloat oldEase) <| easeToFloat ease
                , \{ interval } -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) <| timeIntervalToMinutes interval
                , \{ lapses } -> Expect.equal oldLapses lapses
                ]
                r

        ( Review r, Good ) ->
            Expect.all
                [ \{ ease } -> Expect.within (Absolute 1.0e-9) (easeToFloat oldEase) <| easeToFloat ease
                , \{ interval } -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) <| timeIntervalToMinutes interval
                , \{ lapses } -> Expect.equal oldLapses lapses
                ]
                r

        ( Review r, Hard ) ->
            Expect.all
                [ \{ ease } -> Expect.within (Absolute 1.0e-9) (max 1.3 <| easeToFloat oldEase - 0.15) <| easeToFloat ease
                , \{ interval } -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) <| timeIntervalToMinutes interval
                , \{ lapses } -> Expect.equal oldLapses lapses
                ]
                r

        _ ->
            Expect.fail "Unexpected review update encountered"


{-| Bound an interval by the maximum interval.
-}
boundedDayInterval : AnkiSettings -> TimeInterval Days -> TimeInterval Days
boundedDayInterval { maximumInterval } interval =
    timeIntervalToDays interval
        |> min (timeIntervalToDays maximumInterval)
        |> timeIntervalFromDays


{-| Determine the range an interval in minutes should be fuzzed within.
-}
intervalFuzzRange : AnkiSettings -> Int -> ( Int, Int )
intervalFuzzRange { maximumInterval } interval =
    let
        dayInterval : Int
        dayInterval =
            interval // 1440
    in
    (if dayInterval < 2 then
        ( 1, 1 )

     else if dayInterval == 2 then
        ( min 2 <| timeIntervalToDays maximumInterval
        , min 3 <| timeIntervalToDays maximumInterval
        )

     else
        let
            fuzz : Int
            fuzz =
                if dayInterval < 7 then
                    round << max 1 <| toFloat dayInterval * 0.25

                else if dayInterval < 30 then
                    round << max 2 <| toFloat dayInterval * 0.15

                else
                    round << max 4 <| toFloat dayInterval * 0.05
        in
        ( min (dayInterval - fuzz) <| timeIntervalToDays maximumInterval
        , min (dayInterval + fuzz) <| timeIntervalToDays maximumInterval
        )
    )
        |> Tuple.mapBoth ((*) 1440) ((*) 1440)


{-| Given two different original unfuzzed intervals and fuzzed intervals,
expect the interval in minutes of the former to be greater than the latter (or
fall within the fuzzed range).
-}
expectFuzzedGreaterInterval : AnkiSettings -> ( Int, Int ) -> ( Int, Int ) -> Expectation
expectFuzzedGreaterInterval settings ( refInterval1, interval1 ) ( refInterval2, interval2 ) =
    if interval1 >= interval2 then
        Expect.pass

    else
        -- May be wrong due to fuzzing, so just make sure the ranges are okay
        Expect.all
            [ \() -> Expect.atLeast refInterval2 refInterval1
            , \() -> expectFuzzedInterval settings refInterval1 interval1
            , \() -> expectFuzzedInterval settings refInterval2 interval2
            ]
            ()


{-| Expect an interval to fall within a fuzzed range.
-}
expectFuzzedInterval : AnkiSettings -> Int -> (Int -> Expectation)
expectFuzzedInterval settings interval =
    let
        ( minInt, maxInt ) =
            intervalFuzzRange settings interval
    in
    Expect.all
        [ Expect.atLeast minInt
        , Expect.atMost maxInt
        ]


{-| Determine whether or not a card is a leech.
-}
isCardLeech : AnkiSettings -> { srsData : SRSData } -> Bool
isCardLeech { leechThreshold } c =
    if leechThreshold == Natural.nil then
        False

    else
        case c.srsData of
            Lapsed { lapses } ->
                Natural.toInt lapses >= Natural.toInt leechThreshold

            Review { lapses } ->
                Natural.toInt lapses >= Natural.toInt leechThreshold

            _ ->
                False


{-| Tests for Json encoding/decoding.
-}
suiteJson : Test
suiteJson =
    describe "Json encoding/decoding"
        [ describe "Encode should always be able to be decoded"
            [ fuzz fuzzSRSData "Encode SRSData to string" <|
                \d ->
                    Encode.encode 0
                        (Encode.object [ ( "srsData", encoderSRSData d ) ])
                        |> Decode.decodeString (Decode.field "srsData" decoderSRSData)
                        |> Expect.equal (Ok d)
            , fuzz fuzzSRSData "Encode SRSData to value" <|
                \d ->
                    encoderSRSData d
                        |> Decode.decodeValue decoderSRSData
                        |> Expect.equal (Ok d)
            , fuzz fuzzSettings "Encode AnkiSettings to string" <|
                \s ->
                    Encode.encode 0
                        (Encode.object [ ( "settings", encoderAnkiSettings s ) ])
                        |> Decode.decodeString (Decode.field "settings" decoderAnkiSettings)
                        |> Expect.equal (Ok s)
            , fuzz fuzzSettings "Encode AnkiSettings to value" <|
                \s ->
                    encoderAnkiSettings s
                        |> Decode.decodeValue decoderAnkiSettings
                        |> Expect.equal (Ok s)
            ]
        ]


{-| Tests for `answerCard`.
-}
suiteAnswerCard : Test
suiteAnswerCard =
    describe "answerCard"
        [ fuzz2 fuzzResponse fuzzCard "Card should never be 'New' after answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> Expect.notEqual New
        , fuzz2 fuzzResponse fuzzCard "Time reviewed should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> lastReviewedFromCard
                    |> Expect.equal (Just time)
        , fuzz2 fuzzResponse fuzzCard "Ease should never be < 1.3" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> easeFloatFromCard
                    |> Maybe.map (Expect.atLeast 1.3)
                    |> Maybe.withDefault Expect.pass
        , fuzz2 fuzzResponse fuzzReview "Ease should be updated" <|
            \( time, answer, settings ) old ->
                answerCard time answer settings { srsData = Review old }
                    |> easeFloatFromCard
                    |> Maybe.withDefault -1
                    |> (\newEase ->
                            let
                                oldEase : Float
                                oldEase =
                                    easeToFloat old.ease
                            in
                            case answer of
                                Again ->
                                    boundedLessThan 1.3 oldEase newEase

                                Easy ->
                                    Expect.greaterThan oldEase newEase

                                Good ->
                                    Expect.within (Absolute 1.0e-9) oldEase newEase

                                Hard ->
                                    boundedLessThan 1.3 oldEase newEase
                       )
        , fuzz2 fuzzResponse fuzzCard "Interval should never be < 1 day" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> dayIntervalFromCard
                    |> Maybe.map (Expect.atLeast 1440)
                    |> Maybe.withDefault Expect.pass
        , fuzz2 fuzzResponse fuzzReview "Interval should always be at least 1 day longer for reviewed cards." <|
            \( time, answer, settings ) old ->
                let
                    minBound : Int
                    minBound =
                        timeIntervalToMinutes old.interval
                            + 1440
                            |> min (timeIntervalToMinutes settings.maximumInterval)
                in
                answerCard time answer settings { srsData = Review old }
                    |> (\{ srsData } ->
                            case srsData of
                                Review r ->
                                    expectFuzzedGreaterInterval
                                        settings
                                        ( nextInterval settings answer time old, timeIntervalToMinutes r.interval )
                                        ( minBound, minBound )

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Lapses should be updated and card should be lapsed if appropriate" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> (\{ srsData } ->
                            case ( card.srsData, srsData ) of
                                ( Lapsed old, Lapsed new ) ->
                                    Expect.equal old.lapses new.lapses

                                ( Lapsed old, Review new ) ->
                                    Expect.equal old.lapses new.lapses

                                ( Lapsed _, _ ) ->
                                    Expect.fail "Card should never go from Lapsed to New or Learning."

                                ( Learning _, Lapsed _ ) ->
                                    Expect.fail "Card cannot go from Learning to Lapsed"

                                ( Learning _, _ ) ->
                                    Expect.pass

                                ( New, Lapsed _ ) ->
                                    Expect.fail "Card cannot go from New to Lapsed"

                                ( New, New ) ->
                                    Expect.fail "Card cannot remain new after answering"

                                ( New, _ ) ->
                                    Expect.pass

                                ( Review old, Lapsed new ) ->
                                    Expect.all
                                        [ \() -> Expect.equal Again answer
                                        , \() -> Expect.equal (Natural.succ old.lapses) new.lapses
                                        ]
                                        ()

                                ( Review old, Review new ) ->
                                    Expect.equal old.lapses new.lapses

                                ( Review _, _ ) ->
                                    Expect.fail "Card should never go from Review to New or Learning."
                       )
        , fuzz3 (Fuzz.tuple ( fuzzSettings, fuzzAnswer )) (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzReview "Better answers should always result in longer (or equal) intervals and vice versa" <|
            \( settings, answer1 ) ( time, answer2 ) old ->
                let
                    card : { srsData : SRSData }
                    card =
                        { srsData = Review old }

                    interval1 : Int
                    interval1 =
                        answerCard time answer1 settings card
                            |> reviewIntervalFromCard
                            |> Maybe.withDefault 0

                    interval2 : Int
                    interval2 =
                        answerCard time answer2 settings card
                            |> reviewIntervalFromCard
                            |> Maybe.withDefault 0

                    oneLonger : Expectation
                    oneLonger =
                        expectFuzzedGreaterInterval settings
                            ( nextInterval settings answer1 time old, interval1 )
                            ( nextInterval settings answer2 time old, interval2 )

                    twoLonger : Expectation
                    twoLonger =
                        expectFuzzedGreaterInterval settings
                            ( nextInterval settings answer2 time old, interval2 )
                            ( nextInterval settings answer1 time old, interval1 )
                in
                case ( answer1, answer2 ) of
                    ( Again, Again ) ->
                        Expect.equal interval1 interval2

                    ( Again, _ ) ->
                        twoLonger

                    ( _, Again ) ->
                        oneLonger

                    ( Hard, Hard ) ->
                        Expect.equal interval1 interval2

                    ( Hard, _ ) ->
                        twoLonger

                    ( _, Hard ) ->
                        oneLonger

                    ( Good, Good ) ->
                        Expect.equal interval1 interval2

                    ( Good, Easy ) ->
                        twoLonger

                    ( Easy, Good ) ->
                        oneLonger

                    ( Easy, Easy ) ->
                        Expect.equal interval1 interval2
        , fuzz2 fuzzResponse fuzzDiffOverdueCards "Correct answers for a more overdue card should result in longer intervals and vice versa." <|
            \( time, answer, settings ) ( review1, review2 ) ->
                let
                    overdueAmt1 : Int
                    overdueAmt1 =
                        overdueAmount settings time (Review review1)

                    overdueAmt2 : Int
                    overdueAmt2 =
                        overdueAmount settings time (Review review2)

                    interval1 : Int
                    interval1 =
                        answerCard time answer settings { srsData = Review review1 }
                            |> reviewIntervalFromCard
                            |> Maybe.withDefault 0

                    interval2 : Int
                    interval2 =
                        answerCard time answer settings { srsData = Review review2 }
                            |> reviewIntervalFromCard
                            |> Maybe.withDefault 0
                in
                case ( answer, compare overdueAmt1 overdueAmt2 ) of
                    ( Again, _ ) ->
                        Expect.pass

                    ( _, GT ) ->
                        -- One longer
                        expectFuzzedGreaterInterval settings
                            ( nextInterval settings answer time review1, interval1 )
                            ( nextInterval settings answer time review2, interval2 )

                    ( _, LT ) ->
                        -- Two longer
                        expectFuzzedGreaterInterval settings
                            ( nextInterval settings answer time review2, interval2 )
                            ( nextInterval settings answer time review1, interval1 )

                    ( _, EQ ) ->
                        Expect.equal interval1 interval2
        , fuzz2 fuzzResponse fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .unrelatedField
                    |> Expect.equal card.unrelatedField
        ]


{-| Tests for updating cards in certain queues to the next.
-}
cardSchedulingTests : Test
cardSchedulingTests =
    describe "Card Scheduling"
        [ fuzz fuzzResponse "New cards should be handled appropriately" <|
            \( time, answer, settings ) ->
                answerCard time answer settings { srsData = New }
                    |> (\{ srsData } ->
                            case srsData of
                                Learning { step } ->
                                    Expect.all
                                        [ \() -> Expect.false "Starting steps" <| List.isEmpty settings.newSteps
                                        , \() -> Expect.equal step Natural.nil
                                        ]
                                        ()

                                Review r ->
                                    Expect.all
                                        [ \_ -> Expect.true "No starting steps" <| List.isEmpty settings.newSteps
                                        , \{ ease } -> Expect.within (Absolute 1.0e-9) (easeToFloat settings.startingEase) <| easeToFloat ease
                                        , \{ interval } -> Expect.equal (boundedDayInterval settings settings.graduatingInterval) interval
                                        , \{ lapses } -> Expect.equal Natural.nil lapses
                                        ]
                                        r

                                _ ->
                                    Expect.fail "Card should not go from New to anything but first Learning step or instant graduation with no steps"
                       )
        , fuzz2 fuzzResponse fuzzLearning "Learning cards should be handled appropriately" <|
            \( time, answer, settings ) old ->
                answerCard time answer settings { srsData = Learning old }
                    |> (\{ srsData } ->
                            let
                                normalGraduation : ReviewData -> Expectation
                                normalGraduation =
                                    Expect.all
                                        [ \_ -> Expect.atLeast (List.length settings.newSteps) <| Natural.toInt (Natural.succ old.step)
                                        , \{ ease } -> Expect.within (Absolute 0.000000001) (easeToFloat settings.startingEase) <| easeToFloat ease
                                        , \{ interval } -> Expect.equal (boundedDayInterval settings settings.graduatingInterval) interval
                                        , \{ lapses } -> Expect.equal Natural.nil lapses
                                        ]
                            in
                            case ( srsData, answer ) of
                                ( Learning _, Easy ) ->
                                    Expect.fail "Card should have graduated"

                                ( Learning new, Good ) ->
                                    Expect.all
                                        [ \() ->
                                            Expect.false "Do not advance past learning steps" <| Natural.toInt (Natural.succ old.step) >= List.length settings.newSteps
                                        , \() -> Expect.equal (Natural.succ old.step) new.step
                                        ]
                                        ()

                                ( Learning new, _ ) ->
                                    -- Return to start
                                    Expect.all
                                        [ \_ -> Expect.false "Only return to start if there are new steps" <| List.isEmpty settings.newSteps
                                        , Expect.equal Natural.nil
                                        ]
                                        new.step

                                ( Review new, Easy ) ->
                                    -- Easy graduation
                                    Expect.all
                                        [ \{ ease } -> Expect.within (Absolute 0.000000001) (easeToFloat settings.startingEase) <| easeToFloat ease
                                        , \{ interval } -> Expect.equal (boundedDayInterval settings settings.easyInterval) interval
                                        , \{ lapses } -> Expect.equal Natural.nil lapses
                                        ]
                                        new

                                ( Review new, Good ) ->
                                    normalGraduation new

                                ( Review new, _ ) ->
                                    Expect.all
                                        [ \_ -> Expect.true "Instantly graduate with no new steps" <| List.isEmpty settings.newSteps
                                        , normalGraduation
                                        ]
                                        new

                                _ ->
                                    Expect.fail "Card cannot be New or Lapsed after answering"
                       )
        , fuzz2 fuzzResponse fuzzLapsed "Lapsed cards should be handled appropriately" <|
            \( time, answer, settings ) old ->
                answerCard time answer settings { srsData = Lapsed old }
                    |> (\{ srsData } ->
                            let
                                graduation : ReviewData -> Expectation
                                graduation =
                                    Expect.all
                                        [ \{ ease } -> Expect.equal old.ease ease
                                        , \{ interval } -> Expect.equal (boundedDayInterval settings (minutesToDayInterval <| max (timeIntervalToMinutes settings.lapseMinimumInterval) (truncate <| min 2147483647 (settings.lapseNewInterval * toFloat (timeIntervalToMinutes old.oldInterval))))) interval
                                        , \{ lapses } -> Expect.equal old.lapses lapses
                                        ]
                            in
                            case ( srsData, answer ) of
                                ( Lapsed _, Easy ) ->
                                    Expect.fail "Card should have graduated"

                                ( Lapsed new, Good ) ->
                                    Expect.all
                                        [ \_ -> Expect.lessThan (List.length settings.lapseSteps) <| Natural.toInt (Natural.succ old.step)
                                        , Expect.equal (Natural.succ old.step)
                                        ]
                                        new.step

                                ( Lapsed new, _ ) ->
                                    -- Return to start
                                    Expect.all
                                        [ \_ -> Expect.false "Instantly graduate with no lapse steps" <| List.isEmpty settings.lapseSteps
                                        , \{ ease } ->
                                            Expect.equal old.ease ease
                                        , \{ step } -> Expect.equal Natural.nil step
                                        , \{ oldInterval } ->
                                            Expect.equal old.oldInterval oldInterval
                                        , \{ lapses } ->
                                            Expect.equal old.lapses lapses
                                        ]
                                        new

                                ( Review new, Easy ) ->
                                    graduation new

                                ( Review new, Good ) ->
                                    Expect.all
                                        [ \_ -> Expect.atLeast (List.length settings.lapseSteps) <| Natural.toInt (Natural.succ old.step)
                                        , graduation
                                        ]
                                        new

                                ( Review new, _ ) ->
                                    Expect.all
                                        [ \_ -> Expect.true "Instantly graduate with no lapse steps" <| List.isEmpty settings.lapseSteps
                                        , graduation
                                        ]
                                        new

                                _ ->
                                    Expect.fail "Card cannot go from Lapsed to Learning or New"
                       )
        , fuzz2 fuzzResponse fuzzReview "Review cards should be handled appropriately" <|
            \( time, answer, settings ) old ->
                answerCard time answer settings { srsData = Review old }
                    |> (\{ srsData } ->
                            case srsData of
                                Lapsed new ->
                                    Expect.all
                                        [ \_ -> Expect.equal Again answer
                                        , \{ ease } -> Expect.within (Absolute 1.0e-9) (max 1.3 <| easeToFloat old.ease - 0.2) (easeToFloat ease)
                                        , \{ step } -> Expect.equal Natural.nil step
                                        , \{ oldInterval } -> Expect.equal old.interval oldInterval
                                        , \{ lapses } -> Expect.equal (Natural.succ old.lapses) lapses
                                        ]
                                        new

                                Review _ ->
                                    expectReviewUpdate settings answer time old srsData

                                _ ->
                                    Expect.fail "Card cannot go from Review to Learning or New"
                       )
        ]


{-| Tests for `answerCardInDeck`.
-}
suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Settings shouldn't be touched" <|
            \( time, answer ) deck index ->
                answerCardInDeck time answer index deck
                    |> .settings
                    |> Expect.equal deck.settings
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, answer ) deck index ->
                answerCardInDeck time answer index deck
                    |> .cards
                    |> ArrayX.zip deck.cards
                    |> ArrayX.indexedMapToList (\i ( c1, c2 ) -> i == index || c1 == c2)
                    |> List.all identity
                    |> Expect.true "Only updated card should change in deck"
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck index ->
                let
                    updatedCard : Maybe { srsData : SRSData }
                    updatedCard =
                        Array.get index deck.cards
                            |> Maybe.map (answerCard time answer deck.settings)

                    updatedCardInDeck : Maybe { srsData : SRSData }
                    updatedCardInDeck =
                        answerCardInDeck time answer index deck
                            |> .cards
                            |> Array.get index
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


{-| Tests for `getDueCardIndices`.
-}
suiteGetDue : Test
suiteGetDue =
    describe "getDueCardIndices"
        [ fuzz2 fuzzDeck fuzzTime "Due cards should contain all New cards" <|
            \deck time ->
                let
                    due : List Int
                    due =
                        getDueCardIndices time deck
                in
                Array.toIndexedList deck.cards
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count ((==) New << .srsData << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all cards that are due" <|
            \deck time ->
                let
                    due : List Int
                    due =
                        getDueCardIndices time deck

                    isDue : { srsData : SRSData } -> Bool
                    isDue c =
                        overdueAmount deck.settings time c.srsData >= 0
                in
                Array.toIndexedList deck.cards
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count (isDue << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain any cards that are not (within 20 minutes of being) due" <|
            \deck time ->
                let
                    isNotDue : { srsData : SRSData } -> Bool
                    isNotDue c =
                        overdueAmount deck.settings time c.srsData < -20
                in
                getDueCardIndices time deck
                    |> List.filterMap (\i -> Array.get i deck.cards)
                    |> ListX.count isNotDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        getDueCardIndices time deck
                            |> List.filterMap (\i -> Array.get i deck.cards)

                    firstCard : { srsData : SRSData }
                    firstCard =
                        case List.head dueDeck of
                            Just c ->
                                c

                            Nothing ->
                                { srsData = New }

                    step : { srsData : SRSData } -> ( { srsData : SRSData }, Bool ) -> ( { srsData : SRSData }, Bool )
                    step nextCard ( lastCard, acc ) =
                        let
                            good : ( { srsData : SRSData }, Bool )
                            good =
                                ( nextCard, acc )

                            lastMoreOverdue : ( { srsData : SRSData }, Bool )
                            lastMoreOverdue =
                                ( nextCard, proportionOverdue lastCard >= proportionOverdue nextCard && acc )

                            bad : ( { srsData : SRSData }, Bool )
                            bad =
                                ( nextCard, False )

                            proportionOverdue : { srsData : SRSData } -> Float
                            proportionOverdue c =
                                toFloat (overdueAmount deck.settings time c.srsData) / toFloat (getInterval deck.settings c.srsData)
                        in
                        case ( lastCard.srsData, nextCard.srsData ) of
                            ( New, New ) ->
                                good

                            ( New, _ ) ->
                                bad

                            ( _, New ) ->
                                good

                            ( Learning _, Learning _ ) ->
                                lastMoreOverdue

                            ( Learning _, _ ) ->
                                bad

                            ( _, Learning _ ) ->
                                good

                            ( Review _, Review _ ) ->
                                lastMoreOverdue

                            ( Review _, _ ) ->
                                bad

                            ( _, Review _ ) ->
                                good

                            ( Lapsed _, Lapsed _ ) ->
                                lastMoreOverdue
                in
                dueDeck
                    |> List.foldl step ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]


{-| Tests for `getDueCardIndicesWithDetails`.
-}
suiteGetDueWithDetails : Test
suiteGetDueWithDetails =
    describe "getDueCardIndicesWithDetails"
        [ fuzz2 fuzzDeck fuzzTime "Leech status should be correct" <|
            \deck time ->
                getDueCardIndicesWithDetails time deck
                    |> List.filterMap (\{ index, isLeech } -> Maybe.map (\c -> ( c, isLeech )) <| Array.get index deck.cards)
                    |> List.all
                        (\( c, isLeech ) ->
                            isCardLeech deck.settings c == isLeech
                        )
                    |> Expect.true "Incorrect leech status!"
        , fuzz2 fuzzDeck fuzzTime "Queue status should be correct" <|
            \deck time ->
                let
                    checkQueue : { srsData : SRSData } -> QueueDetails
                    checkQueue c =
                        case c.srsData of
                            Lapsed { step, oldInterval, lastReviewed, lapses } ->
                                LapsedQueue
                                    { formerIntervalInDays = timeIntervalToDays oldInterval
                                    , intervalInMinutes =
                                        ListX.getAt (Natural.toInt step) deck.settings.lapseSteps
                                            |> Maybe.map timeIntervalToMinutes
                                            |> Maybe.withDefault 1
                                    , lapses = Natural.toInt lapses
                                    , lastReviewed = lastReviewed
                                    }

                            Learning { step, lastReviewed } ->
                                LearningQueue
                                    { intervalInMinutes =
                                        ListX.getAt (Natural.toInt step) deck.settings.newSteps
                                            |> Maybe.map timeIntervalToMinutes
                                            |> Maybe.withDefault 1
                                    , lastReviewed = lastReviewed
                                    }

                            New ->
                                NewCard

                            Review { interval, lastReviewed, lapses } ->
                                ReviewQueue
                                    { intervalInDays = timeIntervalToDays interval
                                    , lapses = Natural.toInt lapses
                                    , lastReviewed = lastReviewed
                                    }
                in
                getDueCardIndicesWithDetails time deck
                    |> List.filterMap (\{ index, queueDetails } -> Maybe.map (\c -> ( c, queueDetails )) <| Array.get index deck.cards)
                    |> List.all (\( c, queue ) -> checkQueue c == queue)
                    |> Expect.true "Incorrect queue status!"
        , fuzz2 fuzzDeck fuzzTime "WithDetails should return the same indices in the same order as without" <|
            \deck time ->
                getDueCardIndicesWithDetails time deck
                    |> List.map .index
                    |> Expect.equalLists (getDueCardIndices time deck)
        ]


{-| Tests for `getLeeches`.
-}
suiteGetLeeches : Test
suiteGetLeeches =
    describe "getLeeches"
        [ fuzz fuzzDeck "getLeeches should not return any non-leeches" <|
            \deck ->
                getLeeches deck
                    |> List.filterMap (flip Array.get deck.cards)
                    |> ListX.count (not << isCardLeech deck.settings)
                    |> Expect.equal 0
        , fuzz fuzzDeck "getLeeches should contain all leeches" <|
            \deck ->
                let
                    leeches : List Int
                    leeches =
                        getLeeches deck
                in
                Array.toIndexedList deck.cards
                    |> List.filter (not << flip List.member leeches << Tuple.first)
                    |> ListX.count (isCardLeech deck.settings << Tuple.second)
                    |> Expect.equal 0
        ]
