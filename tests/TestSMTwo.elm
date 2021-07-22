module TestSMTwo exposing
    ( suiteAnswerCard
    , suiteAnswerCardInDeck
    , suiteGetDueCardIndices
    , suiteGetDueCardIndicesWithDetails
    , suiteJson
    )

import Array exposing (Array)
import Array.Extra as ArrayX
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
import Random
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import SpacedRepetition.Internal.SMTwo
    exposing
        ( EFactor
        , ReviewHistory(..)
        , Streak(..)
        , eFactor
        , eFactorToFloat
        , streakToInterval
        )
import SpacedRepetition.SMTwo
    exposing
        ( Answer(..)
        , Card
        , QueueDetails(..)
        , SRSData
        , answerCard
        , answerCardInDeck
        , decoderSRSData
        , encoderSRSData
        , getDueCardIndices
        , getDueCardIndicesWithDetails
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)


{-| Fuzz an EFactor (ease)
-}
fuzzEFactor : Fuzzer EFactor
fuzzEFactor =
    Fuzz.map eFactor (floatRange 0 1000)


{-| Fuzz a streak
-}
fuzzStreak : Fuzzer Streak
fuzzStreak =
    Fuzz.oneOf
        [ Fuzz.constant Zero
        , Fuzz.constant One
        , intRange 6 Random.maxInt
            |> Fuzz.map
                (Natural.fromInt
                    >> Maybe.withDefault Natural.nil
                    >> (\interval -> TwoPlus { interval = interval })
                )
        ]


{-| Fuzz a review history.
-}
fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    Fuzz.oneOf
        [ Fuzz.constant New
        , Fuzz.map3
            (\ease lastReviewed streak ->
                Reviewed
                    { ease = ease
                    , lastReviewed = lastReviewed
                    , streak = streak
                    }
            )
            fuzzEFactor
            fuzzTime
            fuzzStreak
        , Fuzz.map2 (\ease streak -> Repeating { ease = ease, streak = streak })
            fuzzEFactor
            fuzzStreak
        ]


{-| Fuzz a time.
-}
fuzzTime : Fuzzer Time.Posix
fuzzTime =
    Fuzz.map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)


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


{-| Fuzz a card with extra fields.
-}
fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    Fuzz.map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


{-| Fuzz an answer quality.
-}
fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    Fuzz.oneOf
        [ Fuzz.constant Perfect
        , Fuzz.constant
            CorrectWithHesitation
        , Fuzz.constant
            CorrectWithDifficulty
        , Fuzz.constant
            IncorrectButRemembered
        , Fuzz.constant
            IncorrectButFamiliar
        , Fuzz.constant
            NoRecollection
        ]


{-| Get the ease of a card.
-}
eFactorFromCard : Card a -> Float
eFactorFromCard c =
    case c.srsData of
        New ->
            2.5

        Repeating { ease } ->
            eFactorToFloat ease

        Reviewed { ease } ->
            eFactorToFloat ease


{-| Get the streak of a card.
-}
streakFromCard : Card a -> Maybe Streak
streakFromCard c =
    case c.srsData of
        New ->
            Nothing

        Repeating { streak } ->
            Just streak

        Reviewed { streak } ->
            Just streak


{-| Predicate to check if a card is new.
-}
isNew : Card a -> Bool
isNew { srsData } =
    case srsData of
        New ->
            True

        _ ->
            False


{-| Predicate to check if a card is being reviewed.
-}
isReviewed : Card a -> Bool
isReviewed { srsData } =
    case srsData of
        Reviewed _ ->
            True

        _ ->
            False


{-| Predicate to check if a card is being repeated.
-}
isRepeating : Card a -> Bool
isRepeating { srsData } =
    case srsData of
        Repeating _ ->
            True

        _ ->
            False


{-| Convert an Answer to a comparable.
-}
answerToInt : Answer -> Int
answerToInt ans =
    case ans of
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


{-| Expect a new streak to be incremented (and have a longer interval).
-}
expectLonger : Streak -> Streak -> Expectation
expectLonger oldStreak newStreak =
    case ( oldStreak, newStreak ) of
        ( _, Zero ) ->
            Expect.fail "Answered card interval was not incremented."

        ( Zero, One ) ->
            Expect.pass

        ( Zero, TwoPlus _ ) ->
            Expect.fail "Answered card was over-incremented."

        ( One, One ) ->
            Expect.fail "Answered card interval was not incremented."

        ( TwoPlus _, One ) ->
            Expect.fail "Answered card was decremented instead of incremented."

        ( One, TwoPlus _ ) ->
            Expect.pass

        ( TwoPlus old, TwoPlus new ) ->
            Expect.greaterThan (Natural.toInt old.interval)
                (Natural.toInt new.interval)


{-| Given a lower bound and two numbers, ensure that the latter is less than the
former or bounded at the lower bound.
-}
boundedLessThan : Float -> Float -> Float -> Expectation
boundedLessThan bound old new =
    if old == bound then
        Expect.within (Absolute 0.000000001) bound new

    else
        Expect.lessThan old new


{-| Test JSON encoding/decoding.
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


{-| Test `answerCard`.
-}
suiteAnswerCard : Test
suiteAnswerCard =
    describe "answerCard"
        [ fuzz3 fuzzTime fuzzAnswer fuzzCard "Card should never be 'New' after answering" <|
            \time answer card ->
                answerCard time answer card
                    |> .srsData
                    |> Expect.notEqual New
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Time reviewed should be updated" <|
            \time answer card ->
                answerCard time answer card
                    |> (\c ->
                            case c.srsData of
                                Reviewed { lastReviewed } ->
                                    Expect.equal time lastReviewed

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
                                oldEF : Float
                                oldEF =
                                    eFactorFromCard card
                            in
                            case ( isReviewed card, answer ) of
                                ( True, Perfect ) ->
                                    Expect.greaterThan oldEF newEF

                                ( True, CorrectWithHesitation ) ->
                                    Expect.within (Absolute 0.000000001) oldEF newEF

                                ( True, _ ) ->
                                    boundedLessThan 1.3 oldEF newEF

                                ( False, _ ) ->
                                    -- Unchanged EFactor if in repetition phase or new card
                                    Expect.within (Absolute 0.000000001) oldEF newEF
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Streak should be updated and card scheduled" <|
            \time answer card ->
                answerCard time answer card
                    |> streakFromCard
                    |> (\newStreak ->
                            let
                                oldStreak : Streak
                                oldStreak =
                                    streakFromCard card
                                        |> Maybe.withDefault Zero
                            in
                            case answer of
                                CorrectWithDifficulty ->
                                    Expect.equal (Just oldStreak) newStreak

                                CorrectWithHesitation ->
                                    Maybe.withDefault Zero newStreak
                                        |> expectLonger oldStreak

                                IncorrectButFamiliar ->
                                    Expect.equal (Just Zero) newStreak

                                IncorrectButRemembered ->
                                    Expect.equal (Just Zero) newStreak

                                NoRecollection ->
                                    Expect.equal (Just Zero) newStreak

                                Perfect ->
                                    Maybe.withDefault Zero newStreak
                                        |> expectLonger oldStreak
                       )
        , fuzz3 fuzzTime fuzzAnswer fuzzCard "Card should be repeated if necessary" <|
            \time answer card ->
                answerCard time answer card
                    |> (\c ->
                            Expect.true "Card should be repeated if necessary" <|
                                case answer of
                                    CorrectWithHesitation ->
                                        isReviewed c

                                    Perfect ->
                                        isReviewed c

                                    _ ->
                                        isRepeating c
                       )
        , fuzz3 fuzzTime (Fuzz.tuple ( fuzzAnswer, fuzzAnswer )) fuzzCard "Better answers should always result in longer (or equal) intervals and vice versa." <|
            \time ( answer1, answer2 ) card ->
                let
                    firstInterval : Int
                    firstInterval =
                        answerCard time answer1 card
                            |> streakFromCard
                            |> Maybe.map streakToInterval
                            |> Maybe.withDefault Natural.nil
                            |> Natural.toInt

                    secondInterval : Int
                    secondInterval =
                        answerCard time answer2 card
                            |> streakFromCard
                            |> Maybe.map streakToInterval
                            |> Maybe.withDefault Natural.nil
                            |> Natural.toInt
                in
                case compare (answerToInt answer1) (answerToInt answer2) of
                    EQ ->
                        Expect.equal firstInterval secondInterval

                    GT ->
                        Expect.atLeast secondInterval firstInterval

                    LT ->
                        Expect.atLeast firstInterval secondInterval
        , fuzz3 fuzzTime fuzzAnswer fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \time answer card ->
                answerCard time answer card
                    |> .unrelatedField
                    |> Expect.equal card.unrelatedField
        ]


{-| Test `answerCardInDeck`.
-}
suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, answer ) deck index ->
                answerCardInDeck time answer index deck
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
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck i ->
                let
                    updatedCard : Maybe { srsData : SRSData }
                    updatedCard =
                        Array.get i deck
                            |> Maybe.map (answerCard time answer)

                    updatedCardInDeck : Maybe { srsData : SRSData }
                    updatedCardInDeck =
                        answerCardInDeck time answer i deck
                            |> Array.get i
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
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)
                in
                Array.filter (\c -> not <| List.member c dueDeck) deck
                    |> Array.toList
                    |> ListX.count isNew
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Repeating cards" <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)
                in
                Array.filter (\c -> not <| List.member c dueDeck) deck
                    |> Array.toList
                    |> ListX.count isRepeating
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all Reviewed cards that are due" <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - Natural.toFloat interval

                    isDue : { srsData : SRSData } -> Bool
                    isDue c =
                        case c.srsData of
                            Reviewed { lastReviewed, streak } ->
                                overdueAmount (streakToInterval streak) lastReviewed >= 0

                            _ ->
                                True
                in
                Array.filter (\c -> not <| List.member c dueDeck) deck
                    |> Array.toList
                    |> ListX.count isDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain Reviewed cards that are not due" <|
            \deck time ->
                let
                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - Natural.toFloat interval

                    isNotDue : { srsData : SRSData } -> Bool
                    isNotDue c =
                        case c.srsData of
                            Reviewed { lastReviewed, streak } ->
                                overdueAmount (streakToInterval streak) lastReviewed < 0

                            _ ->
                                False
                in
                List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)
                    |> ListX.count isNotDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck) (getDueCardIndices time deck)

                    firstCard : { srsData : SRSData }
                    firstCard =
                        case List.head dueDeck of
                            Just c ->
                                c

                            Nothing ->
                                { srsData = New }

                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount interval reviewed =
                        toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5 - Natural.toFloat interval

                    step : { srsData : SRSData } -> ( { srsData : SRSData }, Bool ) -> ( { srsData : SRSData }, Bool )
                    step nextCard ( lastCard, acc ) =
                        let
                            good : ( { srsData : SRSData }, Bool )
                            good =
                                ( nextCard, acc )

                            bad : ( { srsData : SRSData }, Bool )
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

                            ( Repeating _, Repeating _ ) ->
                                good

                            ( Repeating _, _ ) ->
                                bad

                            ( _, Repeating _ ) ->
                                good

                            ( Reviewed r1, Reviewed r2 ) ->
                                if overdueAmount (streakToInterval r1.streak) r1.lastReviewed >= overdueAmount (streakToInterval r2.streak) r2.lastReviewed then
                                    good

                                else
                                    bad
                in
                dueDeck
                    |> List.foldl step ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]


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

                            Repeating { streak } ->
                                RepeatingQueue { intervalInDays = Natural.toInt <| streakToInterval streak }

                            Reviewed { lastReviewed, streak } ->
                                ReviewQueue
                                    { intervalInDays = Natural.toInt <| streakToInterval streak
                                    , lastReviewed = lastReviewed
                                    }

                    step : ( { srsData : SRSData }, QueueDetails ) -> Bool -> Bool
                    step ( c, queue ) acc =
                        checkQueue c == queue && acc
                in
                getDueCardIndicesWithDetails time deck
                    |> List.filterMap
                        (\{ index, queueDetails } ->
                            Array.get index deck
                                |> Maybe.map (\c -> ( c, queueDetails ))
                        )
                    |> List.foldl step True
                    |> Expect.true "Incorrect queue status!"
        , fuzz2 fuzzDeck fuzzTime "WithDetails should return the same indices in the same order as without" <|
            \deck time ->
                getDueCardIndicesWithDetails time deck
                    |> List.map .index
                    |> Expect.equalLists (getDueCardIndices time deck)
        ]
