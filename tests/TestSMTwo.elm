module TestSMTwo exposing (suiteAnswerCard, suiteAnswerCardInDeck, suiteGetDue, suiteJson)

import Array exposing (Array)
import Array.Extra
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, array, constant, floatRange, int, intRange, map, map2, map3, oneOf)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import SpacedRepetition.Internal.SMTwo exposing (EFactor, ReviewHistory(..), Streak(..), eFactor, eFactorToFloat, streakToInterval)
import SpacedRepetition.SMTwo exposing (Answer(..), Card, SRSData, answerCard, answerCardInDeck, decoderSRSData, encoderSRSData, getDueCardIndices)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)


fuzzEFactor : Fuzzer EFactor
fuzzEFactor =
    map eFactor (floatRange 0 1000)


fuzzStreak : Fuzzer Streak
fuzzStreak =
    oneOf
        [ constant Zero
        , constant One
        , map TwoPlus <| intRange 6 Random.maxInt
        ]


fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    oneOf
        [ constant New
        , map3 Reviewed fuzzEFactor fuzzTime fuzzStreak
        , map2 Repeating fuzzEFactor fuzzStreak
        ]


fuzzTime : Fuzzer Time.Posix
fuzzTime =
    map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)


fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    map (\d -> { srsData = d }) fuzzSRSData


fuzzDeck : Fuzzer (Array { srsData : SRSData })
fuzzDeck =
    array fuzzCard


fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


intToAnswer : Int -> Answer
intToAnswer i =
    case i of
        5 ->
            Perfect

        4 ->
            CorrectWithHesitation

        3 ->
            CorrectWithDifficulty

        2 ->
            IncorrectButRemembered

        1 ->
            IncorrectButFamiliar

        0 ->
            NoRecollection

        _ ->
            NoRecollection


answerToInt : Answer -> Int
answerToInt ans =
    case ans of
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


fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    map intToAnswer <| intRange 0 5


eFactorFromCard : Card a -> Float
eFactorFromCard c =
    case c.srsData of
        New ->
            2.5

        Reviewed eF _ _ ->
            eFactorToFloat eF

        Repeating eF _ ->
            eFactorToFloat eF


streakFromCard : Card a -> Maybe Streak
streakFromCard c =
    case c.srsData of
        New ->
            Nothing

        Reviewed _ _ streak ->
            Just streak

        Repeating _ streak ->
            Just streak


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
        [ fuzz3 fuzzTime fuzzAnswer fuzzCard "Card should never be 'New' after answering" <|
            \time answer card ->
                answerCard time answer card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                New ->
                                    Expect.fail "Card was still 'new' after answering"

                                _ ->
                                    Expect.pass
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Time reviewed should be updated" <|
            \time answer card ->
                answerCard time answer card
                    |> .srsData
                    |> (\rH ->
                            case rH of
                                Reviewed _ rTime _ ->
                                    Expect.equal time rTime

                                _ ->
                                    Expect.pass
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "E-Factor should never be <1.3" <|
            \time answer card ->
                answerCard time answer card
                    |> eFactorFromCard
                    |> Expect.atLeast 1.3
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "E-Factor should be updated" <|
            \time answer card ->
                answerCard time answer card
                    |> eFactorFromCard
                    |> (\newEF ->
                            let
                                oldEF =
                                    eFactorFromCard card

                                boundedLessThan =
                                    if oldEF == 1.3 then
                                        Expect.within (Absolute 0.000000001) 1.3 newEF

                                    else
                                        Expect.lessThan oldEF newEF
                            in
                            case card.srsData of
                                Reviewed _ _ _ ->
                                    case answer of
                                        Perfect ->
                                            Expect.greaterThan oldEF newEF

                                        CorrectWithHesitation ->
                                            Expect.within (Absolute 0.000000001) oldEF newEF

                                        CorrectWithDifficulty ->
                                            boundedLessThan

                                        IncorrectButRemembered ->
                                            boundedLessThan

                                        IncorrectButFamiliar ->
                                            boundedLessThan

                                        NoRecollection ->
                                            boundedLessThan

                                _ ->
                                    -- Unchanged EFactor if in repetition phase or new card
                                    Expect.within (Absolute 0.000000001) oldEF newEF
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Streak should be updated" <|
            \time answer card ->
                answerCard time answer card
                    |> streakFromCard
                    |> (\newStreak ->
                            let
                                oldStreak =
                                    streakFromCard card

                                incremented =
                                    case ( oldStreak, newStreak ) of
                                        ( _, Nothing ) ->
                                            False

                                        ( Nothing, Just newS ) ->
                                            case newS of
                                                Zero ->
                                                    False

                                                _ ->
                                                    True

                                        ( Just oldS, Just newS ) ->
                                            case ( oldS, newS ) of
                                                ( _, Zero ) ->
                                                    False

                                                ( Zero, One ) ->
                                                    True

                                                ( Zero, TwoPlus _ ) ->
                                                    False

                                                ( One, One ) ->
                                                    False

                                                ( One, TwoPlus _ ) ->
                                                    True

                                                ( TwoPlus _, One ) ->
                                                    False

                                                ( TwoPlus oldI, TwoPlus newI ) ->
                                                    if oldI < newI then
                                                        True

                                                    else
                                                        False
                            in
                            case answer of
                                Perfect ->
                                    Expect.true "Expected streak to be incremented" incremented

                                CorrectWithHesitation ->
                                    Expect.true "Expected streak to be incremented" incremented

                                CorrectWithDifficulty ->
                                    -- Never increment streak for CorrectWithDifficulty because it will get incremented when the card graduates from being repeated.
                                    case oldStreak of
                                        Nothing ->
                                            Expect.equal (Just Zero) newStreak

                                        _ ->
                                            Expect.equal oldStreak newStreak

                                IncorrectButRemembered ->
                                    Expect.equal (Just Zero) newStreak

                                IncorrectButFamiliar ->
                                    Expect.equal (Just Zero) newStreak

                                NoRecollection ->
                                    Expect.equal (Just Zero) newStreak
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Card should be repeated if necessary" <|
            \time answer card ->
                answerCard time answer card
                    |> .srsData
                    |> (\rH ->
                            let
                                expectRepeat =
                                    case rH of
                                        New ->
                                            Expect.fail "Repeat expected, got New instead"

                                        Repeating _ _ ->
                                            Expect.pass

                                        Reviewed _ _ _ ->
                                            Expect.fail "Repeat expected, got Reviewed instead"

                                expectReviewed =
                                    case rH of
                                        New ->
                                            Expect.fail "Reviewed expected, got New instead"

                                        Repeating _ _ ->
                                            Expect.fail "Reviewed expected, got Repeating instead"

                                        Reviewed _ _ _ ->
                                            Expect.pass
                            in
                            case answer of
                                Perfect ->
                                    expectReviewed

                                CorrectWithHesitation ->
                                    expectReviewed

                                _ ->
                                    expectRepeat
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Card should be scheduled if appropriate" <|
            \time answer card ->
                answerCard time answer card
                    |> streakFromCard
                    |> (\newStreak ->
                            let
                                oldStreak =
                                    streakFromCard card

                                expectLonger =
                                    case ( oldStreak, newStreak ) of
                                        ( _, Nothing ) ->
                                            Expect.fail "Answered card should never be New"

                                        ( _, Just Zero ) ->
                                            Expect.fail "Answered card interval was not incremented."

                                        ( Just Zero, Just One ) ->
                                            Expect.pass

                                        ( Nothing, Just One ) ->
                                            Expect.pass

                                        ( Just One, Just One ) ->
                                            Expect.fail "Answered card interval was not incremented."

                                        ( Just (TwoPlus _), Just One ) ->
                                            Expect.fail "Answered card was decremented instead of incremented."

                                        ( Just One, Just (TwoPlus _) ) ->
                                            Expect.pass

                                        ( Nothing, Just (TwoPlus _) ) ->
                                            Expect.fail "Answered card was over-incremented."

                                        ( Just Zero, Just (TwoPlus _) ) ->
                                            Expect.fail "Answered card was over-incremented."

                                        ( Just (TwoPlus oldI), Just (TwoPlus newI) ) ->
                                            Expect.greaterThan oldI newI
                            in
                            case answer of
                                Perfect ->
                                    expectLonger

                                CorrectWithHesitation ->
                                    expectLonger

                                _ ->
                                    Expect.pass
                       )
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzAnswer, fuzzAnswer )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa." <|
            \time ( answer1, answer2 ) card ->
                let
                    firstInterval =
                        answerCard time answer1 card
                            |> streakFromCard
                            |> Maybe.map streakToInterval
                            |> Maybe.withDefault -1

                    secondInterval =
                        answerCard time answer2 card
                            |> streakFromCard
                            |> Maybe.map streakToInterval
                            |> Maybe.withDefault -1

                    ansInt1 =
                        answerToInt answer1

                    ansInt2 =
                        answerToInt answer2
                in
                if ansInt1 > ansInt2 then
                    Expect.atLeast secondInterval firstInterval

                else if ansInt2 > ansInt1 then
                    Expect.atLeast firstInterval secondInterval

                else
                    Expect.equal firstInterval secondInterval
        , fuzz3 fuzzTime fuzzAnswer fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \time answer card ->
                answerCard time answer card
                    |> .unrelatedField
                    |> (\i ->
                            Expect.equal card.unrelatedField i
                       )
        ]


suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, answer ) deck index ->
                let
                    updatedDeck =
                        answerCardInDeck time answer index deck
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
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck i ->
                let
                    originalCard =
                        Array.get i deck

                    updatedCard =
                        Maybe.map (answerCard time answer) originalCard

                    updatedDeck =
                        answerCardInDeck time answer i deck

                    updatedCardInDeck =
                        Array.get i updatedDeck
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
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Repeating cards" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck

                    isRepeating c =
                        case c.srsData of
                            Repeating _ _ ->
                                True

                            _ ->
                                False
                in
                notDue
                    |> List.Extra.count isRepeating
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Reviewed cards that are due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck

                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - toFloat interval

                    isDue c =
                        case c.srsData of
                            Reviewed _ reviewed streak ->
                                if overdueAmount (streakToInterval streak) reviewed >= 0 then
                                    True

                                else
                                    False

                            _ ->
                                True
                in
                notDue
                    |> List.Extra.count isDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain Reviewed cards that are not due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - toFloat interval

                    isNotDue c =
                        case c.srsData of
                            Reviewed _ reviewed streak ->
                                if overdueAmount (streakToInterval streak) reviewed >= 0 then
                                    False

                                else
                                    True

                            _ ->
                                False
                in
                List.Extra.count isNotDue dueDeck
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

                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - toFloat interval

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

                            ( Repeating _ _, Repeating _ _ ) ->
                                good

                            ( Repeating _ _, _ ) ->
                                bad

                            ( _, Repeating _ _ ) ->
                                good

                            ( Reviewed _ reviewed1 streak1, Reviewed _ reviewed2 streak2 ) ->
                                if overdueAmount (streakToInterval streak1) reviewed1 >= overdueAmount (streakToInterval streak2) reviewed2 then
                                    good

                                else
                                    bad
                in
                dueDeck
                    |> List.foldl sortCheck ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]
