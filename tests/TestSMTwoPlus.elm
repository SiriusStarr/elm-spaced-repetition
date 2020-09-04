module TestSMTwoPlus exposing
    ( suiteAnswerCard
    , suiteAnswerCardInDeck
    , suiteGetDueCardIndices
    , suiteGetDueCardIndicesWithDetails
    , suiteJson
    )

import Array exposing (Array)
import Array.Extra
import Expect exposing (FloatingPointTolerance(..))
import Fuzz
    exposing
        ( Fuzzer
        , array
        , constant
        , floatRange
        , int
        , intRange
        , map
        , map2
        , map3
        , map4
        , oneOf
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import SpacedRepetition.Internal.SMTwoPlus
    exposing
        ( Difficulty
        , PerformanceRating(..)
        , ReviewHistory(..)
        , createDifficulty
        , createInterval
        , difficultyToFloat
        , intervalToFloat
        , performanceRatingToFloat
        )
import SpacedRepetition.SMTwoPlus
    exposing
        ( Card
        , QueueDetails(..)
        , SRSData
        , answerCard
        , answerCardInDeck
        , decoderSRSData
        , encoderSRSData
        , getDueCardIndices
        , getDueCardIndicesWithDetails
        , oneMinusReciprocalDiffWeightSquared
        , performanceRating
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)


fuzzDifficulty : Fuzzer Difficulty
fuzzDifficulty =
    map createDifficulty (floatRange -0.1 1.1)


fuzzInterval : Fuzzer SpacedRepetition.Internal.SMTwoPlus.Interval
fuzzInterval =
    map createInterval <| floatRange 0 100000


fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    oneOf
        [ constant New
        , map3 Reviewed fuzzDifficulty fuzzTime fuzzInterval
        ]


fuzzDiffOverdueCards : Fuzzer ( { srsData : SRSData }, { srsData : SRSData } )
fuzzDiffOverdueCards =
    map4
        (\diff t1 t2 interval ->
            ( { srsData = Reviewed diff t1 interval }
            , { srsData = Reviewed diff t2 interval }
            )
        )
        fuzzDifficulty
        fuzzTime
        fuzzTime
        fuzzInterval


fuzzTime : Fuzzer Time.Posix
fuzzTime =
    map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)


fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    map (\d -> { srsData = d }) fuzzSRSData


fuzzDeck : Fuzzer (Array { srsData : SRSData })
fuzzDeck =
    array fuzzCard


fuzzPerformance : Fuzzer PerformanceRating
fuzzPerformance =
    map performanceRating <| floatRange -0.1 1.1


fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


difficultyFromCard : Card a -> Float
difficultyFromCard c =
    case c.srsData of
        New ->
            0.3

        Reviewed diff _ _ ->
            difficultyToFloat diff


reviewHistoryToInterval : ReviewHistory -> Maybe Float
reviewHistoryToInterval hist =
    case hist of
        New ->
            Nothing

        Reviewed _ _ interval ->
            Just <| intervalToFloat interval


reviewHistoryToOverdueAmt : Time.Posix -> ReviewHistory -> Maybe Float
reviewHistoryToOverdueAmt time hist =
    case hist of
        New ->
            Nothing

        Reviewed _ reviewed interval ->
            Just <| overdueAmount time (intervalToFloat interval) reviewed


overdueAmount : Time.Posix -> Float -> Time.Posix -> Float
overdueAmount time interval reviewed =
    let
        hourDiff =
            toFloat <| max 0 <| diff Hour Time.utc reviewed time

        overdue =
            toFloat (16 + diff Hour Time.utc reviewed time) / 24 / interval
    in
    if hourDiff <= 8 then
        min 0.99 overdue

    else
        overdue


isDue : Time.Posix -> { srsData : ReviewHistory } -> Bool
isDue time c =
    case reviewHistoryToOverdueAmt time c.srsData of
        Nothing ->
            True

        Just f ->
            f >= 1


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
            ]
        ]


suiteAnswerCard : Test
suiteAnswerCard =
    describe "answerCard"
        [ fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should never be 'New' after answering" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                New ->
                                    Expect.fail "Card was still 'new' after answering"

                                Reviewed _ _ _ ->
                                    Expect.pass
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Time reviewed should be updated" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                Reviewed _ reviewed _ ->
                                    Expect.equal time reviewed

                                New ->
                                    Expect.pass
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Difficulty should always be [0, 1]" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> difficultyFromCard
                    |> Expect.within (Absolute 0.5) 0.5
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Difficulty should be updated" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> difficultyFromCard
                    |> (\newDiff ->
                            let
                                oldDiff =
                                    difficultyFromCard card

                                boundedLessThan =
                                    if zeroPercentDue then
                                        Expect.within (Absolute 0.000000001) oldDiff newDiff

                                    else if oldDiff == 0 then
                                        Expect.within (Absolute 0.000000001) 0 newDiff

                                    else
                                        Expect.lessThan oldDiff newDiff

                                boundedGreaterThan =
                                    if zeroPercentDue then
                                        Expect.within (Absolute 0.000000001) oldDiff newDiff

                                    else if oldDiff == 1 then
                                        Expect.within (Absolute 0.000000001) 1 newDiff

                                    else
                                        Expect.greaterThan oldDiff newDiff

                                lastReviewed =
                                    case card.srsData of
                                        Reviewed _ date _ ->
                                            Just date

                                        New ->
                                            Nothing

                                zeroPercentDue =
                                    case lastReviewed of
                                        Nothing ->
                                            False

                                        Just date ->
                                            if performanceRatingToFloat perf < 0.6 then
                                                False

                                            else
                                                diff Hour Time.utc date time <= 0
                            in
                            case card.srsData of
                                Reviewed _ _ _ ->
                                    -- This magic number is just how the algorithm works out for adjusting difficulty.
                                    if performanceRatingToFloat perf > 0.888888889 then
                                        boundedLessThan

                                    else if performanceRatingToFloat perf < 0.88888888 then
                                        boundedGreaterThan

                                    else
                                        Expect.within (Absolute 0.0001) oldDiff newDiff

                                New ->
                                    -- Unchanged Difficulty if new card
                                    Expect.within (Absolute 0.000000001) oldDiff newDiff
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "daysBetweenReviews should never be < 1" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                New ->
                                    Expect.fail "Answered cards should never be new"

                                Reviewed _ _ interval ->
                                    Expect.atLeast 1 <| intervalToFloat interval
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "daysBetweenReviews should never be < 1 even with a bad scheduling function" <|
            \time perf card ->
                answerCard (Just <| always 0) time perf card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                New ->
                                    Expect.fail "Answered cards should never be new"

                                Reviewed _ _ interval ->
                                    Expect.atLeast 1 <| intervalToFloat interval
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .srsData
                    |> (\rH ->
                            let
                                oldInterval =
                                    case card.srsData of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                newInterval =
                                    case rH of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                expectLonger =
                                    Expect.greaterThan oldInterval newInterval

                                expectShorter =
                                    Expect.lessThan oldInterval newInterval

                                lastReviewed =
                                    case card.srsData of
                                        Reviewed _ date _ ->
                                            Just date

                                        New ->
                                            Nothing

                                zeroPercentDue =
                                    case lastReviewed of
                                        Nothing ->
                                            False

                                        Just date ->
                                            if performanceRatingToFloat perf < 0.6 then
                                                False

                                            else
                                                diff Hour Time.utc date time <= 0
                            in
                            if performanceRatingToFloat perf >= 0.6 then
                                if zeroPercentDue then
                                    Expect.within (Absolute 0.000000001) oldInterval newInterval

                                else
                                    expectLonger

                            else if oldInterval == 0 then
                                Expect.equal newInterval 1

                            else if oldInterval == 1 then
                                Expect.equal oldInterval newInterval

                            else
                                expectShorter
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled with bad scheduling function" <|
            \time perf card ->
                answerCard (Just <| always 0) time perf card
                    |> .srsData
                    |> (\rH ->
                            let
                                oldInterval =
                                    case card.srsData of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                newInterval =
                                    case rH of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                expectLonger =
                                    Expect.greaterThan oldInterval newInterval

                                expectShorter =
                                    Expect.lessThan oldInterval newInterval

                                lastReviewed =
                                    case card.srsData of
                                        Reviewed _ date _ ->
                                            Just date

                                        New ->
                                            Nothing

                                zeroPercentDue =
                                    case lastReviewed of
                                        Nothing ->
                                            False

                                        Just date ->
                                            if performanceRatingToFloat perf < 0.6 then
                                                False

                                            else
                                                diff Hour Time.utc date time <= 0
                            in
                            if performanceRatingToFloat perf >= 0.6 then
                                if zeroPercentDue then
                                    Expect.within (Absolute 0.000000001) oldInterval newInterval

                                else
                                    expectLonger

                            else if oldInterval == 0 then
                                Expect.equal newInterval 1

                            else if oldInterval == 1 then
                                Expect.equal oldInterval newInterval

                            else
                                expectShorter
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled with good scheduling func" <|
            \time perf card ->
                answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf card
                    |> .srsData
                    |> (\rH ->
                            let
                                oldInterval =
                                    case card.srsData of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                newInterval =
                                    case rH of
                                        New ->
                                            0

                                        Reviewed _ _ interval ->
                                            intervalToFloat interval

                                expectLonger =
                                    Expect.greaterThan oldInterval newInterval

                                expectShorter =
                                    Expect.lessThan oldInterval newInterval

                                lastReviewed =
                                    case card.srsData of
                                        Reviewed _ date _ ->
                                            Just date

                                        New ->
                                            Nothing

                                zeroPercentDue =
                                    case lastReviewed of
                                        Nothing ->
                                            False

                                        Just date ->
                                            if performanceRatingToFloat perf < 0.6 then
                                                False

                                            else
                                                diff Hour Time.utc date time <= 0
                            in
                            if performanceRatingToFloat perf >= 0.6 then
                                if zeroPercentDue then
                                    Expect.within (Absolute 0.000000001) oldInterval newInterval

                                else
                                    expectLonger

                            else if oldInterval == 0 then
                                Expect.equal newInterval 1

                            else if oldInterval == 1 then
                                Expect.equal oldInterval newInterval

                            else
                                expectShorter
                       )
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzPerformance, fuzzPerformance )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa, except bad incorrect behavior." <|
            \time ( perf1, perf2 ) card ->
                let
                    firstInterval =
                        answerCard Nothing time perf1 card
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    secondInterval =
                        answerCard Nothing time perf2 card
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    ansFloat1 =
                        performanceRatingToFloat perf1

                    ansFloat2 =
                        performanceRatingToFloat perf2
                in
                if ansFloat1 > ansFloat2 then
                    if ansFloat1 < 0.6 && ansFloat2 < 0.6 then
                        Expect.atLeast firstInterval secondInterval

                    else
                        Expect.atLeast secondInterval firstInterval

                else if ansFloat2 > ansFloat1 then
                    if ansFloat1 < 0.6 && ansFloat2 < 0.6 then
                        Expect.atLeast secondInterval firstInterval

                    else
                        Expect.atLeast firstInterval secondInterval

                else
                    Expect.within (Absolute 0.000000001) firstInterval secondInterval
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzPerformance, fuzzPerformance )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa with a good scheduling func" <|
            \time ( perf1, perf2 ) card ->
                let
                    firstInterval =
                        answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf1 card
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    secondInterval =
                        answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf2 card
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    ansFloat1 =
                        performanceRatingToFloat perf1

                    ansFloat2 =
                        performanceRatingToFloat perf2
                in
                if ansFloat1 > ansFloat2 then
                    Expect.atLeast secondInterval firstInterval

                else if ansFloat2 > ansFloat1 then
                    Expect.atLeast firstInterval secondInterval

                else
                    Expect.within (Absolute 0.000000001) firstInterval secondInterval
        , fuzz3 fuzzTime fuzzPerformance fuzzDiffOverdueCards "Correct answers for a more overdue card should result in longer intervals (up to 2x) and vice versa." <|
            \time answer ( card1, card2 ) ->
                let
                    overdueAmt1 =
                        min 2 <| Maybe.withDefault 0 <| reviewHistoryToOverdueAmt time <| card1.srsData

                    overdueAmt2 =
                        min 2 <| Maybe.withDefault 0 <| reviewHistoryToOverdueAmt time <| card2.srsData

                    firstInterval =
                        answerCard Nothing time answer card1
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    firstDifficulty =
                        answerCard Nothing time answer card1
                            |> difficultyFromCard

                    secondInterval =
                        answerCard Nothing time answer card2
                            |> .srsData
                            |> reviewHistoryToInterval
                            |> Maybe.withDefault -1

                    secondDifficulty =
                        answerCard Nothing time answer card2
                            |> difficultyFromCard

                    ansFloat =
                        performanceRatingToFloat answer
                in
                if ansFloat < 0.6 then
                    Expect.pass

                else if overdueAmt1 > overdueAmt2 then
                    if firstDifficulty > secondDifficulty then
                        -- This is a weird edge case produced by the algorithm, where at poor performance ratings, the increase in difficulty from a long-overdue answer outweighs the gain in interval length.
                        Expect.pass

                    else
                        Expect.atLeast secondInterval firstInterval

                else if overdueAmt2 > overdueAmt1 then
                    if secondDifficulty > firstDifficulty then
                        -- This is a weird edge case produced by the algorithm, where at poor performance ratings, the increase in difficulty from a long-overdue answer outweighs the gain in interval length.
                        Expect.pass

                    else
                        Expect.atLeast firstInterval secondInterval

                else
                    Expect.within (Absolute 0.000000001) firstInterval secondInterval
        , fuzz3 fuzzTime fuzzPerformance fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .unrelatedField
                    |> (\i ->
                            Expect.equal card.unrelatedField i
                       )
        ]


suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzPerformance )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, perf ) deck index ->
                let
                    updatedDeck =
                        answerCardInDeck Nothing time perf index deck
                in
                Array.Extra.zip deck updatedDeck
                    |> Array.Extra.indexedMapToList
                        (\i ( c1, c2 ) ->
                            if i == index then
                                True

                            else
                                c1 == c2
                        )
                    |> List.all identity
                    |> Expect.true "Only updated card should change in deck"
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzPerformance )) fuzzDeck Fuzz.int "Answering card by index should be the same as answering independently" <|
            \( time, perf ) deck index ->
                let
                    originalCard =
                        Array.get index deck

                    updatedCard =
                        Maybe.map (answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf) originalCard

                    updatedDeck =
                        answerCardInDeck (Just oneMinusReciprocalDiffWeightSquared) time perf index deck

                    updatedCardInDeck =
                        Array.get index updatedDeck
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


suiteGetDueCardIndices : Test
suiteGetDueCardIndices =
    describe "getDueCardIndices"
        [ fuzz2 fuzzDeck fuzzTime "Due cards should contain all New cards" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck

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
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Reviewed cards that are due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck
                in
                notDue
                    |> List.Extra.count (isDue time)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain any Reviewed cards that are not due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    isNotDue =
                        not << isDue time
                in
                dueDeck
                    |> List.Extra.count isNotDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

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
                        in
                        case ( lastCard.srsData, nextCard.srsData ) of
                            ( New, New ) ->
                                good

                            ( New, _ ) ->
                                bad

                            ( _, New ) ->
                                good

                            ( Reviewed _ reviewed1 interval1, Reviewed _ reviewed2 interval2 ) ->
                                if overdueAmount time (intervalToFloat interval1) reviewed1 >= overdueAmount time (intervalToFloat interval2) reviewed2 then
                                    good

                                else
                                    bad
                in
                dueDeck
                    |> List.foldl sortCheck ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]


suiteGetDueCardIndicesWithDetails : Test
suiteGetDueCardIndicesWithDetails =
    describe "getDueCardIndicesWithDetails"
        [ fuzz2 fuzzDeck fuzzTime "Queue status should be correct" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\{ index, queueDetails } -> Maybe.map (\c -> ( c, queueDetails )) <| Array.get index deck) <| getDueCardIndicesWithDetails time deck

                    checkQueue c =
                        case c.srsData of
                            Reviewed _ lastSeen interval ->
                                ReviewQueue
                                    { lastSeen = lastSeen
                                    , intervalInDays = intervalToFloat interval
                                    }

                            New ->
                                NewCard

                    queueCheck ( c, queue ) goodSort =
                        if checkQueue c == queue then
                            goodSort

                        else
                            False
                in
                dueDeck
                    |> List.foldl queueCheck True
                    |> Expect.true "Incorrect queue status!"
        , fuzz2 fuzzDeck fuzzTime "WithDetails should return the same indices in the same order as without" <|
            \deck time ->
                getDueCardIndicesWithDetails time deck
                    |> List.map .index
                    |> Expect.equalLists (getDueCardIndices time deck)
        ]
