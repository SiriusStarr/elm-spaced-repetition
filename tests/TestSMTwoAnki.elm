module TestSMTwoAnki exposing
    ( suiteAnswerCard
    , suiteAnswerCardInDeck
    , suiteGetDue
    , suiteGetLeeches
    , suiteJson
    )

import Array exposing (Array)
import Array.Extra
import Basics.Extra exposing (flip)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
    exposing
        ( Fuzzer
        , andMap
        , array
        , constant
        , floatRange
        , int
        , intRange
        , map
        , map2
        , map3
        , map4
        , map5
        , oneOf
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import SpacedRepetition.Internal.SMTwoAnki
    exposing
        ( Days
        , Ease
        , Lapses
        , Minutes
        , QueueStatus(..)
        , Step(..)
        , TimeInterval(..)
        , createEase
        , createLapses
        , createStep
        , createTimeIntervalInDays
        , createTimeIntervalInMinutes
        , easeToFloat
        , lapsesToInt
        , minutesToDayInterval
        , stepToInt
        , timeIntervalToDays
        , timeIntervalToMinutes
        )
import SpacedRepetition.SMTwoAnki
    exposing
        ( AnkiSettings
        , Answer(..)
        , SRSData
        , answerCard
        , answerCardInDeck
        , decoderAnkiSettings
        , decoderSRSData
        , encoderAnkiSettings
        , encoderSRSData
        , getDueCardIndices
        , getLeeches
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)


fuzzEase : Fuzzer Ease
fuzzEase =
    map createEase (floatRange 0 1000)


fuzzLapses : Fuzzer Lapses
fuzzLapses =
    map createLapses (intRange -1 1000)


fuzzStep : Fuzzer Step
fuzzStep =
    map createStep (intRange -1 1000)


fuzzTime : Fuzzer Time.Posix
fuzzTime =
    map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)


fuzzMinuteInterval : Fuzzer (TimeInterval Minutes)
fuzzMinuteInterval =
    map createTimeIntervalInMinutes <| intRange 0 52560000


fuzzDayInterval : Fuzzer (TimeInterval Days)
fuzzDayInterval =
    map createTimeIntervalInDays <| intRange 0 36500


fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    oneOf
        [ constant New
        , map2 Learning fuzzStep fuzzTime
        , map4 Review fuzzEase fuzzDayInterval fuzzTime fuzzLapses
        , map5 Lapsed fuzzEase fuzzStep fuzzDayInterval fuzzTime fuzzLapses
        ]


fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    map (\d -> { srsData = d }) fuzzSRSData


fuzzDeck : Fuzzer { cards : Array { srsData : SRSData }, settings : AnkiSettings }
fuzzDeck =
    map2 (\c s -> { cards = c, settings = s }) (array fuzzCard) fuzzSettings


fuzzSettings : Fuzzer AnkiSettings
fuzzSettings =
    map AnkiSettings (Fuzz.list fuzzMinuteInterval)
        |> andMap fuzzDayInterval
        |> andMap fuzzDayInterval
        |> andMap (floatRange 0 1000)
        |> andMap (floatRange 0 1000)
        |> andMap (floatRange 0 1000)
        |> andMap fuzzDayInterval
        |> andMap (floatRange 0 1000)
        |> andMap (Fuzz.list fuzzMinuteInterval)
        |> andMap (floatRange 0 1000)
        |> andMap fuzzDayInterval
        |> andMap int


fuzzDiffOverdueCards : Fuzzer ( { srsData : SRSData }, { srsData : SRSData } )
fuzzDiffOverdueCards =
    map5
        (\ease interval t1 t2 lapses ->
            ( { srsData = Review ease interval t1 lapses }
            , { srsData = Review ease interval t2 lapses }
            )
        )
        fuzzEase
        fuzzDayInterval
        fuzzTime
        fuzzTime
        fuzzLapses


fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    map
        (\i ->
            case i of
                0 ->
                    Again

                1 ->
                    Hard

                2 ->
                    Good

                3 ->
                    Easy

                _ ->
                    Again
        )
    <|
        intRange 0 3


fuzzResponse : Fuzzer ( Time.Posix, Answer, AnkiSettings )
fuzzResponse =
    map3 (\t a s -> ( t, a, s )) fuzzTime fuzzAnswer fuzzSettings


fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


easeFloatFromCard : { srsData : SRSData } -> Maybe Float
easeFloatFromCard c =
    case c.srsData of
        Review ease _ _ _ ->
            Just <| easeToFloat ease

        Lapsed ease _ _ _ _ ->
            Just <| easeToFloat ease

        _ ->
            Nothing


getInterval : AnkiSettings -> SRSData -> Int
getInterval settings srsData =
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

        Review _ i _ _ ->
            timeIntervalToMinutes i


overdueAmount : AnkiSettings -> Time.Posix -> SRSData -> Int
overdueAmount settings time srsData =
    let
        reviewed =
            case srsData of
                New ->
                    Time.millisToPosix 0

                Learning _ t ->
                    t

                Lapsed _ _ _ t _ ->
                    t

                Review _ _ t _ ->
                    t

        interval =
            getInterval settings srsData

        minuteDiff : Int
        minuteDiff =
            diff Minute Time.utc reviewed time
    in
    case srsData of
        New ->
            0

        _ ->
            minuteDiff - interval


nextInterval : AnkiSettings -> Answer -> Time.Posix -> SRSData -> Int
nextInterval settings answer time oldData =
    let
        ( oldEase, oldInterval ) =
            case oldData of
                Review e i _ _ ->
                    ( easeToFloat e, i )

                _ ->
                    ( 0, createTimeIntervalInDays 1 )

        newEase =
            case answer of
                Hard ->
                    max 1.3 <| oldEase - 0.15

                Good ->
                    oldEase

                Easy ->
                    oldEase + 0.15

                Again ->
                    0

        oldIntervalInMinutes =
            timeIntervalToMinutes oldInterval

        modifier =
            case answer of
                Again ->
                    0

                Hard ->
                    min newEase settings.hardInterval

                Good ->
                    newEase

                Easy ->
                    newEase * max 1 settings.easyBonus

        scaleInterval =
            case answer of
                Again ->
                    0

                Hard ->
                    oldIntervalInMinutes

                Good ->
                    overdueAmount settings time oldData
                        |> (//) 2
                        |> (+) oldIntervalInMinutes
                        |> max oldIntervalInMinutes

                Easy ->
                    overdueAmount settings time oldData
                        |> (+) oldIntervalInMinutes
                        |> max oldIntervalInMinutes
    in
    -- This magic number is max int; truncate does not play well with larger values.
    max (truncate <| min 2147483647 <| modifier * settings.intervalModifier * toFloat scaleInterval) (scaleInterval + 1440)


expectReviewUpdate : AnkiSettings -> Answer -> Time.Posix -> SRSData -> SRSData -> Expectation
expectReviewUpdate settings answer time oldData newData =
    let
        ( oldEase, oldLapses ) =
            case oldData of
                Review e _ _ l ->
                    ( e, l )

                _ ->
                    ( createEase 0, createLapses 0 )

        ( newEase, newInterval, newLapses ) =
            case newData of
                Review e i _ l ->
                    ( e, i, l )

                _ ->
                    ( createEase 0, createTimeIntervalInDays 0, createLapses 0 )

        floatEase =
            easeToFloat newEase

        intInterval =
            timeIntervalToDays newInterval

        intLapses =
            lapsesToInt newLapses

        expectation =
            case answer of
                Again ->
                    always <| Expect.fail "Card should have lapsed"

                Good ->
                    Expect.all
                        [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (easeToFloat oldEase) e
                        , \( _, i, _ ) -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) i
                        , \( _, _, l ) -> Expect.equal (lapsesToInt oldLapses) l
                        ]

                Easy ->
                    Expect.all
                        [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (0.15 + easeToFloat oldEase) e
                        , \( _, i, _ ) -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) i
                        , \( _, _, l ) -> Expect.equal (lapsesToInt oldLapses) l
                        ]

                Hard ->
                    Expect.all
                        [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (max 1.3 <| easeToFloat oldEase - 0.15) e
                        , \( _, i, _ ) -> (expectFuzzedInterval settings <| nextInterval settings answer time oldData) i
                        , \( _, _, l ) -> Expect.equal (lapsesToInt oldLapses) l
                        ]
    in
    expectation <| ( floatEase, intInterval, intLapses )


boundedDayInterval : AnkiSettings -> TimeInterval Days -> TimeInterval Days
boundedDayInterval { maximumInterval } interval =
    timeIntervalToDays interval
        |> min (timeIntervalToDays maximumInterval)
        |> createTimeIntervalInDays


fuzzRange : AnkiSettings -> Int -> ( Int, Int )
fuzzRange { maximumInterval } interval =
    let
        dayInterval =
            interval // 1440

        fuzz =
            if dayInterval < 7 then
                round << max 1 <| toFloat dayInterval * 0.25

            else if dayInterval < 30 then
                round << max 2 <| toFloat dayInterval * 0.15

            else
                round << max 4 <| toFloat dayInterval * 0.05
    in
    if dayInterval < 2 then
        ( 1, 1 )

    else if dayInterval == 2 then
        ( min 2 <| timeIntervalToDays maximumInterval
        , min 3 <| timeIntervalToDays maximumInterval
        )

    else
        ( min (dayInterval - fuzz) <| timeIntervalToDays maximumInterval
        , min (dayInterval + fuzz) <| timeIntervalToDays maximumInterval
        )


expectFuzzedInterval : AnkiSettings -> Int -> (Int -> Expectation)
expectFuzzedInterval settings interval =
    let
        ( minInt, maxInt ) =
            fuzzRange settings interval
    in
    Expect.all
        [ Expect.atLeast minInt
        , Expect.atMost maxInt
        ]


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


{-| Tests for answerCard.
-}
suiteAnswerCard : Test
suiteAnswerCard =
    describe "answerCard"
        [ fuzz2 fuzzResponse fuzzCard "Card should never be 'New' after answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\queueStatus ->
                            case queueStatus of
                                New ->
                                    Expect.fail "Card was still 'new' after answering"

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Time reviewed should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\queueStatus ->
                            case queueStatus of
                                Learning _ rTime ->
                                    Expect.equal time rTime

                                Review _ _ rTime _ ->
                                    Expect.equal time rTime

                                Lapsed _ _ _ rTime _ ->
                                    Expect.equal time rTime

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Ease should never be < 1.3" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> easeFloatFromCard
                    |> (\ease ->
                            case ease of
                                Nothing ->
                                    Expect.pass

                                Just f ->
                                    Expect.atLeast 1.3 f
                       )
        , fuzz2 fuzzResponse fuzzCard "Ease should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> easeFloatFromCard
                    |> Maybe.withDefault -1
                    |> (\newEase ->
                            let
                                oldEase =
                                    Maybe.withDefault -1 <| easeFloatFromCard card

                                boundedLessThan =
                                    if oldEase == 1.3 then
                                        Expect.within (Absolute 0.000000001) 1.3 newEase

                                    else
                                        Expect.lessThan oldEase newEase
                            in
                            case card.srsData of
                                Review _ _ _ _ ->
                                    case answer of
                                        Again ->
                                            boundedLessThan

                                        Hard ->
                                            boundedLessThan

                                        Good ->
                                            Expect.within (Absolute 0.000000001) oldEase newEase

                                        Easy ->
                                            Expect.greaterThan oldEase newEase

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Interval should never be < 1 day" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            case qS of
                                Review _ interval _ _ ->
                                    Expect.atLeast 1440 <| timeIntervalToMinutes interval

                                Lapsed _ _ interval _ _ ->
                                    Expect.atLeast 1440 <| timeIntervalToMinutes interval

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Interval should always be at least 1 day longer for reviewed cards." <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            let
                                oldInterval =
                                    case card.srsData of
                                        Review _ interval _ _ ->
                                            timeIntervalToMinutes interval

                                        _ ->
                                            0

                                ( minBound, _ ) =
                                    fuzzRange settings oldInterval
                            in
                            case qS of
                                Review _ interval _ _ ->
                                    Expect.atLeast minBound <| timeIntervalToMinutes interval

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Lapses should be updated and card should be lapsed if appropriate" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            case card.srsData of
                                New ->
                                    case qS of
                                        Lapsed _ _ _ _ _ ->
                                            Expect.fail "Card cannot go from New to Lapsed"

                                        _ ->
                                            Expect.pass

                                Learning _ _ ->
                                    case qS of
                                        Lapsed _ _ _ _ _ ->
                                            Expect.fail "Card cannot go from Learning to Lapsed"

                                        _ ->
                                            Expect.pass

                                Review _ _ _ oldLapses ->
                                    case qS of
                                        Lapsed _ _ _ _ newLapses ->
                                            if answer /= Again then
                                                Expect.fail "Card should only lapse if answer is Again."

                                            else
                                                Expect.equal (lapsesToInt oldLapses + 1) (lapsesToInt newLapses)

                                        Review _ _ _ newLapses ->
                                            Expect.equal oldLapses newLapses

                                        _ ->
                                            Expect.fail "Card should never go from Review to New or Learning."

                                Lapsed _ _ _ _ oldLapses ->
                                    case qS of
                                        Lapsed _ _ _ _ newLapses ->
                                            Expect.equal oldLapses newLapses

                                        Review _ _ _ newLapses ->
                                            Expect.equal oldLapses newLapses

                                        _ ->
                                            Expect.fail "Card should never go from Lapsed to New or Learning."
                       )
        , fuzz2 fuzzResponse fuzzCard "New cards should be handled appropriately" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            case card.srsData of
                                New ->
                                    if List.isEmpty settings.newSteps then
                                        case qS of
                                            Review ease interval _ lapses ->
                                                Expect.all
                                                    [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (max 1.3 settings.startingEase) <| easeToFloat e
                                                    , \( _, i, _ ) -> Expect.equal (boundedDayInterval settings settings.graduatingInterval) i
                                                    , \( _, _, l ) -> Expect.equal (createLapses 0) l
                                                    ]
                                                    ( ease, interval, lapses )

                                            _ ->
                                                Expect.fail "Card should graduate instantly if no newSteps"

                                    else
                                        case qS of
                                            Learning step _ ->
                                                Expect.equal step <| createStep 0

                                            _ ->
                                                Expect.fail "Card should not go from New to anything but first Learning step"

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Learning cards should be handled appropriately" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            let
                                returnToStart step =
                                    Expect.equal (createStep 0) step

                                normalGraduation ease interval lapses =
                                    Expect.all
                                        [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (max 1.3 settings.startingEase) <| easeToFloat e
                                        , \( _, i, _ ) -> Expect.equal (boundedDayInterval settings settings.graduatingInterval) i
                                        , \( _, _, l ) -> Expect.equal (createLapses 0) l
                                        ]
                                        ( ease, interval, lapses )

                                easyGraduation ease interval lapses =
                                    Expect.all
                                        [ \( e, _, _ ) -> Expect.within (Absolute 0.000000001) (max 1.3 settings.startingEase) <| easeToFloat e
                                        , \( _, i, _ ) -> Expect.equal (boundedDayInterval settings settings.easyInterval) i
                                        , \( _, _, l ) -> Expect.equal (createLapses 0) l
                                        ]
                                        ( ease, interval, lapses )
                            in
                            case card.srsData of
                                Learning oldStep _ ->
                                    case qS of
                                        Lapsed _ _ _ _ _ ->
                                            Expect.fail "Card cannot go from Learning to Lapsed"

                                        New ->
                                            Expect.fail "Card cannot go from Learning to New"

                                        Learning newStep _ ->
                                            case answer of
                                                Again ->
                                                    returnToStart newStep

                                                Hard ->
                                                    returnToStart newStep

                                                Good ->
                                                    if stepToInt oldStep + 1 >= List.length settings.newSteps then
                                                        Expect.fail "Card should have graduated"

                                                    else
                                                        Expect.equal (stepToInt oldStep + 1) <| stepToInt newStep

                                                Easy ->
                                                    Expect.fail "Card should have graduated"

                                        Review ease interval _ lapses ->
                                            case answer of
                                                Again ->
                                                    Expect.fail "Card should not have graduated"

                                                Hard ->
                                                    Expect.fail "Card should not have graduated"

                                                Good ->
                                                    if stepToInt oldStep + 1 >= List.length settings.newSteps then
                                                        normalGraduation ease interval lapses

                                                    else
                                                        Expect.fail "Card should not have graduated"

                                                Easy ->
                                                    easyGraduation ease interval lapses

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Lapsed cards should be handled appropriately" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            case card.srsData of
                                Lapsed oldEase oldStep oldInterval _ oldLapses ->
                                    let
                                        returnToStart newEase newStep newInterval newLapses =
                                            Expect.all
                                                [ \{ ease } ->
                                                    Expect.equal oldEase ease
                                                , \{ step } -> Expect.equal (createStep 0) step
                                                , \{ interval } ->
                                                    Expect.equal oldInterval interval
                                                , \{ lapses } ->
                                                    Expect.equal oldLapses lapses
                                                ]
                                                { ease = newEase, step = newStep, interval = newInterval, lapses = newLapses }

                                        graduation ease interval lapses =
                                            Expect.all
                                                [ \( e, _, _ ) -> Expect.equal oldEase e
                                                , \( _, i, _ ) -> Expect.equal (boundedDayInterval settings (minutesToDayInterval <| max (timeIntervalToMinutes settings.lapseMinimumInterval) (truncate <| min 2147483647 (settings.lapseNewInterval * toFloat (timeIntervalToMinutes oldInterval))))) i
                                                , \( _, _, l ) -> Expect.equal oldLapses l
                                                ]
                                                ( ease, interval, lapses )
                                    in
                                    case qS of
                                        New ->
                                            Expect.fail "Card cannot go from Lapsed to New"

                                        Learning _ _ ->
                                            Expect.fail "Card cannot go from Lapsed to Learning"

                                        Lapsed newEase newStep newInterval _ newLapses ->
                                            case answer of
                                                Again ->
                                                    returnToStart newEase newStep newInterval newLapses

                                                Hard ->
                                                    returnToStart newEase newStep newInterval newLapses

                                                Good ->
                                                    if stepToInt oldStep + 1 >= List.length settings.lapseSteps then
                                                        Expect.fail "Card should have graduated"

                                                    else
                                                        Expect.equal (stepToInt oldStep + 1) <| stepToInt newStep

                                                Easy ->
                                                    Expect.fail "Card should have graduated"

                                        Review ease interval _ lapses ->
                                            if List.isEmpty settings.lapseSteps then
                                                graduation ease interval lapses

                                            else
                                                case answer of
                                                    Again ->
                                                        Expect.fail "Card should not have graduated"

                                                    Hard ->
                                                        Expect.fail "Card should not have graduated"

                                                    Good ->
                                                        if stepToInt oldStep + 1 >= List.length settings.lapseSteps then
                                                            graduation ease interval lapses

                                                        else
                                                            Expect.fail "Card should not have graduated"

                                                    Easy ->
                                                        graduation ease interval lapses

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Review cards should be handled appropriately" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\qS ->
                            case card.srsData of
                                Review oldEase oldInterval _ oldLapses ->
                                    case qS of
                                        New ->
                                            Expect.fail "Card cannot go from Review to New"

                                        Learning _ _ ->
                                            Expect.fail "Card cannot go from Review to Learning"

                                        Lapsed newEase newStep newInterval _ newLapses ->
                                            case answer of
                                                Again ->
                                                    Expect.all
                                                        [ \{ ease } ->
                                                            Expect.within (Absolute 0.000000001) (max 1.3 <| easeToFloat oldEase - 0.2) (easeToFloat ease)
                                                        , \{ step } -> Expect.equal (createStep 0) step
                                                        , \{ interval } ->
                                                            Expect.equal oldInterval interval
                                                        , \{ lapses } ->
                                                            Expect.equal (1 + lapsesToInt oldLapses) (lapsesToInt lapses)
                                                        ]
                                                        { ease = newEase, step = newStep, interval = newInterval, lapses = newLapses }

                                                _ ->
                                                    Expect.fail "Card should not have lapsed"

                                        Review _ _ _ _ ->
                                            expectReviewUpdate settings answer time card.srsData qS

                                _ ->
                                    Expect.pass
                       )
        , fuzz3 (Fuzz.tuple ( fuzzSettings, fuzzAnswer )) (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa" <|
            \( settings, answer1 ) ( time, answer2 ) card ->
                let
                    interval1 =
                        answerCard time answer1 settings card
                            |> .srsData
                            |> (\qS ->
                                    case qS of
                                        Review _ i _ _ ->
                                            timeIntervalToMinutes i

                                        _ ->
                                            0
                               )

                    interval2 =
                        answerCard time answer2 settings card
                            |> .srsData
                            |> (\qS ->
                                    case qS of
                                        Review _ i _ _ ->
                                            timeIntervalToMinutes i

                                        _ ->
                                            0
                               )

                    oneLonger =
                        if interval1 >= interval2 then
                            Expect.pass

                        else
                            -- May be wrong due to fuzzing, so just make sure the ranges are okay
                            Expect.atLeast (Tuple.first <| fuzzRange settings (nextInterval settings answer2 time card.srsData)) (Tuple.first <| fuzzRange settings (nextInterval settings answer1 time card.srsData))

                    twoLonger =
                        if interval2 >= interval1 then
                            Expect.pass

                        else
                            Expect.atLeast (Tuple.first <| fuzzRange settings (nextInterval settings answer1 time card.srsData)) (Tuple.first <| fuzzRange settings (nextInterval settings answer2 time card.srsData))
                in
                case card.srsData of
                    Review _ _ _ _ ->
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

                    _ ->
                        Expect.pass
        , fuzz2 fuzzResponse fuzzDiffOverdueCards "Correct answers for a more overdue card should result in longer intervals and vice versa." <|
            \( time, answer, settings ) ( card1, card2 ) ->
                let
                    overdueAmt1 =
                        overdueAmount settings time card1.srsData

                    overdueAmt2 =
                        overdueAmount settings time card2.srsData

                    interval1 =
                        answerCard time answer settings card1
                            |> .srsData
                            |> (\qS ->
                                    case qS of
                                        Review _ i _ _ ->
                                            timeIntervalToMinutes i

                                        _ ->
                                            0
                               )

                    interval2 =
                        answerCard time answer settings card2
                            |> .srsData
                            |> (\qS ->
                                    case qS of
                                        Review _ i _ _ ->
                                            timeIntervalToMinutes i

                                        _ ->
                                            0
                               )
                in
                case answer of
                    Again ->
                        Expect.pass

                    _ ->
                        if overdueAmt1 > overdueAmt2 then
                            if interval1 >= interval2 then
                                Expect.pass

                            else
                                Expect.atLeast (Tuple.first <| fuzzRange settings (nextInterval settings answer time card2.srsData)) (Tuple.first <| fuzzRange settings (nextInterval settings answer time card1.srsData))

                        else if overdueAmt2 > overdueAmt1 then
                            if interval2 >= interval1 then
                                Expect.pass

                            else
                                Expect.atLeast (Tuple.first <| fuzzRange settings (nextInterval settings answer time card1.srsData)) (Tuple.first <| fuzzRange settings (nextInterval settings answer time card2.srsData))

                        else
                            Expect.equal interval1 interval2
        , fuzz2 fuzzResponse fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .unrelatedField
                    |> (\i ->
                            Expect.equal card.unrelatedField i
                       )
        ]


suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck Fuzz.int "Settings shouldn't be touched" <|
            \( time, answer ) deck index ->
                let
                    originalSettings =
                        deck.settings

                    updatedDeck =
                        answerCardInDeck time answer index deck

                    updatedSettings =
                        updatedDeck.settings
                in
                Expect.equal originalSettings updatedSettings
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, answer ) deck index ->
                let
                    updatedDeck =
                        answerCardInDeck time answer index deck
                in
                Array.Extra.zip deck.cards updatedDeck.cards
                    |> Array.Extra.indexedMapToList
                        (\i ( c1, c2 ) ->
                            if i == index then
                                True

                            else
                                c1 == c2
                        )
                    |> List.all identity
                    |> Expect.true "Only updated card should change in deck"
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck Fuzz.int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck index ->
                let
                    originalCard =
                        Array.get index deck.cards

                    updatedCard =
                        Maybe.map (answerCard time answer deck.settings) originalCard

                    updatedDeck =
                        answerCardInDeck time answer index deck

                    updatedCardInDeck =
                        Array.get index updatedDeck.cards
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


suiteGetDue : Test
suiteGetDue =
    describe "getDue"
        [ fuzz2 fuzzDeck fuzzTime "Due cards should contain all New cards" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\( i, _ ) -> Array.get i deck.cards) <| getDueCardIndices time deck

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck.cards

                    isNew c =
                        case c.srsData of
                            New ->
                                True

                            _ ->
                                False
                in
                notDue
                    |> List.Extra.count isNew
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all cards that are due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\( i, _ ) -> Array.get i deck.cards) <| getDueCardIndices time deck

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck.cards

                    isDue c =
                        overdueAmount deck.settings time c.srsData >= 0
                in
                notDue
                    |> List.Extra.count isDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain any cards that are not (within 20 minutes of being) due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\( i, _ ) -> Array.get i deck.cards) <| getDueCardIndices time deck

                    isNotDue c =
                        overdueAmount deck.settings time c.srsData < -20
                in
                dueDeck
                    |> List.Extra.count isNotDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\( i, _ ) -> Array.get i deck.cards) <| getDueCardIndices time deck

                    firstCard =
                        case List.head dueDeck of
                            Nothing ->
                                { srsData = New }

                            Just c ->
                                c

                    sortCheck nextCard ( lastCard, goodSort ) =
                        let
                            good =
                                if goodSort then
                                    ( nextCard, True )

                                else
                                    ( nextCard, False )

                            bad =
                                ( nextCard, False )

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

                            ( Learning _ _, Learning _ _ ) ->
                                if proportionOverdue lastCard >= proportionOverdue nextCard then
                                    good

                                else
                                    bad

                            ( Learning _ _, _ ) ->
                                bad

                            ( _, Learning _ _ ) ->
                                good

                            ( Review _ _ _ _, Review _ _ _ _ ) ->
                                if proportionOverdue lastCard >= proportionOverdue nextCard then
                                    good

                                else
                                    bad

                            ( Review _ _ _ _, _ ) ->
                                bad

                            ( _, Review _ _ _ _ ) ->
                                good

                            ( Lapsed _ _ _ _ _, Lapsed _ _ _ _ _ ) ->
                                if proportionOverdue lastCard >= proportionOverdue nextCard then
                                    good

                                else
                                    bad
                in
                dueDeck
                    |> List.foldl sortCheck ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        , fuzz2 fuzzDeck fuzzTime "Leech status should be correct" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\( i, leechStatus ) -> Maybe.map (\c -> ( c, leechStatus )) <| Array.get i deck.cards) <| getDueCardIndices time deck

                    isLeech c =
                        if deck.settings.leechThreshold <= 0 then
                            False

                        else
                            case c.srsData of
                                Review _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                Lapsed _ _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                _ ->
                                    False

                    leechCheck ( c, leechStatus ) goodSort =
                        if isLeech c == leechStatus then
                            goodSort

                        else
                            False
                in
                dueDeck
                    |> List.foldl leechCheck True
                    |> Expect.true "Incorrect leech status!"
        ]


suiteGetLeeches : Test
suiteGetLeeches =
    describe "getLeeches"
        [ fuzz2 fuzzDeck fuzzTime "getLeeches should not return any non-leeches" <|
            \deck _ ->
                let
                    leechDeck =
                        List.filterMap (flip Array.get deck.cards) <| getLeeches deck

                    isLeech c =
                        if deck.settings.leechThreshold <= 0 then
                            False

                        else
                            case c.srsData of
                                Review _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                Lapsed _ _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                _ ->
                                    False
                in
                leechDeck
                    |> List.Extra.count (not << isLeech)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "getLeeches should contain all leeches" <|
            \deck _ ->
                let
                    leechDeck =
                        List.filterMap (flip Array.get deck.cards) <| getLeeches deck

                    notLeech =
                        Array.toList <| Array.filter (\c -> not <| List.member c leechDeck) deck.cards

                    isLeech c =
                        if deck.settings.leechThreshold <= 0 then
                            False

                        else
                            case c.srsData of
                                Review _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                Lapsed _ _ _ _ lapses ->
                                    lapsesToInt lapses >= deck.settings.leechThreshold

                                _ ->
                                    False
                in
                notLeech
                    |> List.Extra.count isLeech
                    |> Expect.equal 0
        ]
