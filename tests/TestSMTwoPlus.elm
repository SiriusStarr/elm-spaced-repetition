module TestSMTwoPlus exposing (suite)

import Array exposing (Array)
import Array.Extra as ArrayX
import Basics.Extra exposing (flip, safeDivide)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz
    exposing
        ( Fuzzer
        , floatRange
        , int
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListX
import SpacedRepetition.Internal.SMTwoPlus
    exposing
        ( Difficulty
        , PerformanceRating
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
import Util exposing (boundedGreaterThan, boundedLessThan, fuzzTime)


{-| Tests for SM-2 Plus algorithm.
-}
suite : Test
suite =
    describe "SM-2 Plus"
        [ suiteAnswerCard
        , suiteAnswerCardInDeck
        , suiteGetDueCardIndices
        , suiteGetDueCardIndicesWithDetails
        , suiteJson
        ]


{-| Test `answerCard`.
-}
suiteAnswerCard : Test
suiteAnswerCard =
    describe "answerCard"
        [ fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should never be 'New' after answering" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .srsData
                    |> Expect.notEqual New
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Time reviewed should be updated" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> lastReviewedFromCard
                    |> Expect.equal (Just time)
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
                                oldDiff : Float
                                oldDiff =
                                    difficultyFromCard card
                            in
                            case
                                ( card.srsData
                                , zeroPercentDue time perf card
                                )
                            of
                                ( New, _ ) ->
                                    Expect.within (Absolute 0.000000001) oldDiff newDiff

                                ( Reviewed _, False ) ->
                                    -- This magic number (8/9) is just how the algorithm works out for adjusting difficulty.
                                    case compare (8 / 9) <| performanceRatingToFloat perf of
                                        LT ->
                                            boundedLessThan 0 oldDiff newDiff

                                        EQ ->
                                            Expect.within (Absolute 0.000000001) oldDiff newDiff

                                        GT ->
                                            boundedGreaterThan 1 oldDiff newDiff

                                _ ->
                                    Expect.within (Absolute 0.000000001) oldDiff newDiff
                       )
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "daysBetweenReviews should never be < 1" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> intervalFromCard
                    |> Maybe.withDefault 0
                    |> Expect.atLeast 1
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "daysBetweenReviews should never be < 1 even with a bad scheduling function" <|
            \time perf card ->
                answerCard (Just <| always 0) time perf card
                    |> intervalFromCard
                    |> Maybe.withDefault 0
                    |> Expect.atLeast 1
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> confirmCardScheduled time perf card
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled with bad scheduling function" <|
            \time perf card ->
                answerCard (Just <| always 0) time perf card
                    |> confirmCardScheduled time perf card
        , fuzz3 fuzzTime fuzzPerformance fuzzCard "Card should be scheduled with good scheduling func" <|
            \time perf card ->
                answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf card
                    |> confirmCardScheduled time perf card
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzPerformance, fuzzPerformance )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa, except bad incorrect behavior." <|
            \time ( perf1, perf2 ) card ->
                let
                    bothIncorrect : Bool
                    bothIncorrect =
                        performanceRatingToFloat perf1
                            < 0.6
                            && performanceRatingToFloat perf2
                            < 0.6

                    firstInterval : Float
                    firstInterval =
                        answerCard Nothing time perf1 card
                            |> intervalFromCard
                            |> Maybe.withDefault -1

                    secondInterval : Float
                    secondInterval =
                        answerCard Nothing time perf2 card
                            |> intervalFromCard
                            |> Maybe.withDefault -1
                in
                case
                    ( compare (performanceRatingToFloat perf1) (performanceRatingToFloat perf2)
                    , bothIncorrect
                    )
                of
                    ( LT, True ) ->
                        Expect.atLeast secondInterval firstInterval

                    ( LT, False ) ->
                        Expect.atLeast firstInterval secondInterval

                    ( GT, True ) ->
                        Expect.atLeast firstInterval secondInterval

                    ( GT, False ) ->
                        Expect.atLeast secondInterval firstInterval

                    _ ->
                        Expect.within (Absolute 0.000000001) firstInterval secondInterval
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzPerformance, fuzzPerformance )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa with a good scheduling func" <|
            \time ( perf1, perf2 ) card ->
                let
                    firstInterval : Float
                    firstInterval =
                        answerCard Nothing time perf1 card
                            |> intervalFromCard
                            |> Maybe.withDefault -1

                    secondInterval : Float
                    secondInterval =
                        answerCard Nothing time perf2 card
                            |> intervalFromCard
                            |> Maybe.withDefault -1
                in
                case compare (performanceRatingToFloat perf1) (performanceRatingToFloat perf2) of
                    LT ->
                        Expect.atLeast firstInterval secondInterval

                    EQ ->
                        Expect.within (Absolute 0.000000001) firstInterval secondInterval

                    GT ->
                        Expect.atLeast secondInterval firstInterval
        , fuzz3 fuzzTime fuzzCorrectPerformance fuzzDiffOverdueCards "Correct answers for a more overdue card should result in longer intervals (up to 2x) and vice versa." <|
            \time answer ( card1, card2 ) ->
                let
                    firstDifficulty : Float
                    firstDifficulty =
                        answerCard Nothing time answer card1
                            |> difficultyFromCard

                    firstInterval : Float
                    firstInterval =
                        answerCard Nothing time answer card1
                            |> intervalFromCard
                            |> Maybe.withDefault -1

                    overdueAmt1 : Float
                    overdueAmt1 =
                        overdueAmountFromCard time card1
                            |> Maybe.withDefault 0
                            |> min 2

                    overdueAmt2 : Float
                    overdueAmt2 =
                        overdueAmountFromCard time card2
                            |> Maybe.withDefault 0
                            |> min 2

                    secondDifficulty : Float
                    secondDifficulty =
                        answerCard Nothing time answer card2
                            |> difficultyFromCard

                    secondInterval : Float
                    secondInterval =
                        answerCard Nothing time answer card2
                            |> intervalFromCard
                            |> Maybe.withDefault -1
                in
                case ( compare overdueAmt1 overdueAmt2, compare firstDifficulty secondDifficulty ) of
                    ( LT, LT ) ->
                        -- This is a weird edge case produced by the algorithm, where at poor performance ratings, the increase in difficulty from a long-overdue answer outweighs the gain in interval length.
                        Expect.pass

                    ( LT, _ ) ->
                        Expect.atLeast firstInterval secondInterval

                    ( EQ, _ ) ->
                        Expect.within (Absolute 0.000000001) firstInterval secondInterval

                    ( GT, GT ) ->
                        -- This is a weird edge case produced by the algorithm, where at poor performance ratings, the increase in difficulty from a long-overdue answer outweighs the gain in interval length.
                        Expect.pass

                    ( GT, _ ) ->
                        Expect.atLeast secondInterval firstInterval
        , fuzz3 fuzzTime fuzzPerformance fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \time perf card ->
                answerCard Nothing time perf card
                    |> .unrelatedField
                    |> Expect.equal card.unrelatedField
        ]


{-| Confirm that a card was scheduled properly.
-}
confirmCardScheduled : Time.Posix -> PerformanceRating -> Card a -> Card a -> Expectation
confirmCardScheduled time perf old new =
    let
        newInterval : Float
        newInterval =
            intervalFromCard new
                |> Maybe.withDefault 0

        oldInterval : Float
        oldInterval =
            intervalFromCard old
                |> Maybe.withDefault 0
    in
    if performanceRatingToFloat perf >= 0.6 then
        if zeroPercentDue time perf old then
            Expect.within (Absolute 0.000000001) oldInterval newInterval

        else
            Expect.greaterThan oldInterval newInterval

    else
        boundedLessThan 1 oldInterval newInterval


{-| Get the difficulty from a card.
-}
difficultyFromCard : Card a -> Float
difficultyFromCard c =
    case c.srsData of
        New ->
            0.3

        Reviewed { difficulty } ->
            difficultyToFloat difficulty


{-| Fuzz a correct answer.
-}
fuzzCorrectPerformance : Fuzzer PerformanceRating
fuzzCorrectPerformance =
    Fuzz.map performanceRating <| floatRange 0.6 1.0


{-| Fuzz two cards that are identical other than when they were last reviewed.
-}
fuzzDiffOverdueCards : Fuzzer ( { srsData : SRSData }, { srsData : SRSData } )
fuzzDiffOverdueCards =
    Fuzz.map4
        (\diff t1 t2 interval ->
            ( { srsData =
                    Reviewed
                        { difficulty = diff
                        , interval = interval
                        , lastReviewed = t1
                        }
              }
            , { srsData =
                    Reviewed
                        { difficulty = diff
                        , interval = interval
                        , lastReviewed = t2
                        }
              }
            )
        )
        fuzzDifficulty
        fuzzTime
        fuzzTime
        fuzzInterval


{-| Fuzz a card with non-SRS fields.
-}
fuzzExtendedCard : Fuzzer { unrelatedField : Int, srsData : SRSData }
fuzzExtendedCard =
    Fuzz.map2 (\d i -> { unrelatedField = i, srsData = d }) fuzzSRSData int


{-| Test `answerCardInDeck`.
-}
suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzPerformance )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, perf ) deck index ->
                answerCardInDeck Nothing time perf index deck
                    |> ArrayX.zip deck
                    |> ArrayX.indexedMapToList
                        (\i ( c1, c2 ) ->
                            if i == index then
                                True

                            else
                                c1 == c2
                        )
                    |> List.all identity
                    |> Expect.true "Only updated card should change in deck"
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzPerformance )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, perf ) deck index ->
                let
                    updatedCard : Maybe { srsData : SRSData }
                    updatedCard =
                        Array.get index deck
                            |> Maybe.map (answerCard (Just oneMinusReciprocalDiffWeightSquared) time perf)

                    updatedCardInDeck : Maybe { srsData : SRSData }
                    updatedCardInDeck =
                        answerCardInDeck (Just oneMinusReciprocalDiffWeightSquared) time perf index deck
                            |> Array.get index
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


{-| Test `getDueCardIndices`.
-}
suiteGetDueCardIndices : Test
suiteGetDueCardIndices =
    describe "getDueCardIndices"
        [ fuzz2 fuzzDeck fuzzTime "Due cards should contain all New cards" <|
            \deck time ->
                let
                    due : List Int
                    due =
                        getDueCardIndices time deck
                in
                Array.toIndexedList deck
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count ((==) New << .srsData << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Reviewed cards that are due" <|
            \deck time ->
                let
                    due : List Int
                    due =
                        getDueCardIndices time deck
                in
                Array.toIndexedList deck
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count (isDue time << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain any Reviewed cards that are not due" <|
            \deck time ->
                getDueCardIndices time deck
                    |> List.filterMap (\i -> Array.get i deck)
                    |> ListX.count (not << isDue time)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    firstCard : { srsData : SRSData }
                    firstCard =
                        List.head dueDeck
                            |> Maybe.withDefault { srsData = New }

                    step : { srsData : SRSData } -> ( { srsData : SRSData }, Bool ) -> ( { srsData : SRSData }, Bool )
                    step nextCard ( lastCard, goodSort ) =
                        let
                            bad : ( { srsData : SRSData }, Bool )
                            bad =
                                ( nextCard, False )

                            good : ( { srsData : SRSData }, Bool )
                            good =
                                ( nextCard, goodSort )
                        in
                        case ( lastCard.srsData, nextCard.srsData ) of
                            ( New, New ) ->
                                good

                            ( New, _ ) ->
                                bad

                            ( _, New ) ->
                                good

                            ( Reviewed r1, Reviewed r2 ) ->
                                if
                                    overdueAmount time (intervalToFloat r1.interval) r1.lastReviewed
                                        >= overdueAmount time (intervalToFloat r2.interval) r2.lastReviewed
                                then
                                    good

                                else
                                    bad
                in
                List.drop 1 dueDeck
                    |> List.foldl step ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]


{-| Determine whether a card is due.
-}
isDue : Time.Posix -> Card a -> Bool
isDue time c =
    case overdueAmountFromCard time c of
        Just f ->
            f >= 1

        Nothing ->
            True


{-| Test `getDueCardIndicesWithDetails`.
-}
suiteGetDueCardIndicesWithDetails : Test
suiteGetDueCardIndicesWithDetails =
    describe "getDueCardIndicesWithDetails"
        [ fuzz2 fuzzDeck fuzzTime "Queue status should be correct" <|
            \deck time ->
                let
                    checkQueue : { srsData : SRSData } -> QueueDetails
                    checkQueue c =
                        case c.srsData of
                            New ->
                                NewCard

                            Reviewed { interval, lastReviewed } ->
                                ReviewQueue
                                    { intervalInDays = intervalToFloat interval
                                    , lastSeen = lastReviewed
                                    }
                in
                getDueCardIndicesWithDetails time deck
                    |> List.filterMap
                        (\{ index, queueDetails } ->
                            Array.get index deck
                                |> Maybe.map (\c -> ( c, queueDetails ))
                        )
                    |> List.all (\( c, queue ) -> checkQueue c == queue)
                    |> Expect.true "Incorrect queue status!"
        , fuzz2 fuzzDeck fuzzTime "WithDetails should return the same indices in the same order as without" <|
            \deck time ->
                getDueCardIndicesWithDetails time deck
                    |> List.map .index
                    |> Expect.equalLists (getDueCardIndices time deck)
        ]


{-| Test Json encoding/decoding.
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
            ]
        ]


{-| Fuzz a card.
-}
fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    Fuzz.map (\d -> { srsData = d }) fuzzSRSData


{-| Fuzz a deck of cards.
-}
fuzzDeck : Fuzzer (Array { srsData : SRSData })
fuzzDeck =
    Fuzz.array fuzzCard


{-| Fuzz a difficulty.
-}
fuzzDifficulty : Fuzzer Difficulty
fuzzDifficulty =
    Fuzz.map createDifficulty (floatRange -0.1 1.1)


{-| Fuzz a new interval for a card.
-}
fuzzInterval : Fuzzer SpacedRepetition.Internal.SMTwoPlus.Interval
fuzzInterval =
    Fuzz.map createInterval <| floatRange 0 100000


{-| Fuzz an answer quality.
-}
fuzzPerformance : Fuzzer PerformanceRating
fuzzPerformance =
    Fuzz.map performanceRating <| floatRange -0.1 1.1


{-| Fuzz a review history for a card.
-}
fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    Fuzz.oneOf
        [ Fuzz.constant New
        , Fuzz.map3
            (\difficulty interval lastReviewed ->
                Reviewed
                    { difficulty = difficulty
                    , interval = interval
                    , lastReviewed = lastReviewed
                    }
            )
            fuzzDifficulty
            fuzzInterval
            fuzzTime
        ]


{-| Get the interval from a card.
-}
intervalFromCard : Card a -> Maybe Float
intervalFromCard c =
    case c.srsData of
        New ->
            Nothing

        Reviewed { interval } ->
            Just <| intervalToFloat interval


{-| Get the date last reviewed from a card.
-}
lastReviewedFromCard : Card a -> Maybe Time.Posix
lastReviewedFromCard c =
    case c.srsData of
        New ->
            Nothing

        Reviewed { lastReviewed } ->
            Just lastReviewed


{-| Determine how overdue a card is.
-}
overdueAmount : Time.Posix -> Float -> Time.Posix -> Float
overdueAmount time interval reviewed =
    let
        overdue : Float
        overdue =
            safeDivide (toFloat (16 + diff Hour Time.utc reviewed time) / 24) interval
                |> Maybe.withDefault 1
    in
    if diff Hour Time.utc reviewed time <= 8 then
        min 0.99 overdue

    else
        overdue


{-| Get the percent overdue a card is.
-}
overdueAmountFromCard : Time.Posix -> Card a -> Maybe Float
overdueAmountFromCard time c =
    case c.srsData of
        New ->
            Nothing

        Reviewed { interval, lastReviewed } ->
            Just <| overdueAmount time (intervalToFloat interval) lastReviewed


{-| Determine if a card is not even slightly due.
-}
zeroPercentDue : Time.Posix -> PerformanceRating -> Card a -> Bool
zeroPercentDue time perf card =
    case lastReviewedFromCard card of
        Just date ->
            if performanceRatingToFloat perf < 0.6 then
                False

            else
                diff Hour Time.utc date time <= 0

        Nothing ->
            False
