module TestLeitner exposing
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
        , int
        , intRange
        , map
        , map2
        , map3
        , oneOf
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Random
import SpacedRepetition.Internal.Leitner exposing (Box(..), NumberOfBoxes(..))
import SpacedRepetition.Leitner
    exposing
        ( Answer(..)
        , LeitnerSettings
        , OnIncorrect(..)
        , QueueDetails(..)
        , SRSData
        , SpacingFunction
        , answerCard
        , answerCardInDeck
        , decoderSRSData
        , doubleSpacing
        , encoderSRSData
        , fibonacciSpacing
        , getDueCardIndices
        , getDueCardIndicesWithDetails
        , numberOfBoxes
        )
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Time
import Time.Extra exposing (Interval(..), diff)


fuzzOnIncorrect : Fuzzer OnIncorrect
fuzzOnIncorrect =
    oneOf
        [ constant BackOneBox
        , constant BackToStart
        ]


fuzzSpacing : Fuzzer SpacingFunction
fuzzSpacing =
    oneOf
        [ constant fibonacciSpacing
        , constant doubleSpacing
        , constant alternateSpacingFunction
        ]


{-| A stand-in for a user-provided spacing function.
-}
alternateSpacingFunction : SpacingFunction
alternateSpacingFunction i =
    i + 1


fuzzNumBoxes : Fuzzer NumberOfBoxes
fuzzNumBoxes =
    map numberOfBoxes <| intRange -1 Random.maxInt


fuzzSettings : Fuzzer LeitnerSettings
fuzzSettings =
    map3 LeitnerSettings fuzzOnIncorrect fuzzSpacing fuzzNumBoxes


fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    oneOf
        [ constant New
        , map2 BoxN (intRange -1 Random.maxInt) fuzzTime
        , constant Graduated
        ]


fuzzTime : Fuzzer Time.Posix
fuzzTime =
    map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)


fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    map (\d -> { srsData = d }) fuzzSRSData


fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


fuzzDeck :
    Fuzzer
        { cards : Array { srsData : SRSData }
        , settings : LeitnerSettings
        }
fuzzDeck =
    let
        makeDeck c s =
            { cards = c, settings = s }
    in
    map2 makeDeck (array fuzzCard) fuzzSettings


fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    oneOf
        [ constant Correct
        , constant Incorrect
        , constant Pass
        , map MoveBoxes int
        , constant BackToFirstBox
        ]


fuzzResponse : Fuzzer ( Time.Posix, Answer, LeitnerSettings )
fuzzResponse =
    map3 (\t a s -> ( t, a, s )) fuzzTime fuzzAnswer fuzzSettings


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
                    |> (\box ->
                            case box of
                                New ->
                                    Expect.fail "Card was still 'new' after answering"

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Time reviewed should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\box ->
                            case box of
                                New ->
                                    Expect.fail "Card was still 'new' after answering"

                                Graduated ->
                                    Expect.pass

                                BoxN _ reviewedTime ->
                                    Expect.equal time reviewedTime
                       )
        , fuzz2 fuzzResponse fuzzCard "Box should never be < 0 or greater than max boxes" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\box ->
                            let
                                maxBox =
                                    case settings.numBoxes of
                                        NumberOfBoxes i ->
                                            i
                            in
                            case box of
                                BoxN boxNum _ ->
                                    Expect.all
                                        [ Expect.atLeast 0
                                        , Expect.lessThan maxBox
                                        ]
                                        boxNum

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Box should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\box ->
                            let
                                maxBox =
                                    case settings.numBoxes of
                                        NumberOfBoxes i ->
                                            i

                                oldBox =
                                    case card.srsData of
                                        BoxN boxNum _ ->
                                            -- Things that are over max boxes or <0 will get fixed when answered.
                                            clamp 0 maxBox boxNum

                                        New ->
                                            0

                                        Graduated ->
                                            maxBox

                                newBox =
                                    case box of
                                        BoxN boxNum _ ->
                                            boxNum

                                        New ->
                                            -1

                                        Graduated ->
                                            maxBox

                                expectMoved i =
                                    Expect.equal (clamp 0 maxBox <| oldBox + i) newBox

                                expectFirstBox =
                                    Expect.equal 0 newBox
                            in
                            case answer of
                                Correct ->
                                    expectMoved 1

                                Incorrect ->
                                    case settings.onIncorrect of
                                        BackOneBox ->
                                            expectMoved -1

                                        BackToStart ->
                                            expectFirstBox

                                Pass ->
                                    Expect.equal oldBox newBox

                                MoveBoxes i ->
                                    expectMoved i

                                BackToFirstBox ->
                                    expectFirstBox
                       )
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
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Settings shouldn't be touched" <|
            \( time, answer ) deck i ->
                let
                    updatedDeck =
                        answerCardInDeck time answer i deck

                    updatedSettings =
                        updatedDeck.settings
                in
                Expect.equal deck.settings updatedSettings
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
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck i ->
                let
                    originalCard =
                        Array.get i deck.cards

                    updatedCard =
                        Maybe.map (answerCard time answer deck.settings) originalCard

                    updatedDeck =
                        answerCardInDeck time answer i deck

                    updatedCardInDeck =
                        Array.get i updatedDeck.cards
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


suiteGetDueCardIndices : Test
suiteGetDueCardIndices =
    describe "getDue"
        [ fuzz2 fuzzDeck fuzzTime "Due cards should contain all New cards" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

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
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain Graduated cards" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

                    isGraduated c =
                        case c.srsData of
                            Graduated ->
                                True

                            _ ->
                                False
                in
                dueDeck
                    |> List.Extra.count isGraduated
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all cards that are due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

                    notDue =
                        Array.toList <| Array.filter (\c -> not <| List.member c dueDeck) deck.cards

                    overdueAmount box reviewed =
                        (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5) / toFloat (boxInterval box)

                    boxInterval i =
                        if i > 0 then
                            deck.settings.boxSpacing i

                        else
                            1

                    isDue c =
                        case c.srsData of
                            New ->
                                True

                            Graduated ->
                                False

                            BoxN box reviewed ->
                                overdueAmount box reviewed >= 1
                in
                notDue
                    |> List.Extra.count isDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain cards that are not due" <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

                    overdueAmount box reviewed =
                        (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5) / toFloat (boxInterval box)

                    boxInterval i =
                        if i > 0 then
                            deck.settings.boxSpacing i

                        else
                            1

                    isDue c =
                        case c.srsData of
                            New ->
                                True

                            Graduated ->
                                False

                            BoxN box reviewed ->
                                overdueAmount box reviewed >= 1

                    isNotDue c =
                        not <| isDue c
                in
                dueDeck
                    |> List.Extra.count isNotDue
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

                    firstCard =
                        case List.head dueDeck of
                            Nothing ->
                                { srsData = New }

                            Just c ->
                                c

                    overdueAmount box reviewed =
                        (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5) / toFloat (boxInterval box)

                    boxInterval i =
                        if i > 0 then
                            deck.settings.boxSpacing i

                        else
                            1

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
                            ( Graduated, _ ) ->
                                -- Shouldn't be graduated cards, period.
                                bad

                            ( _, Graduated ) ->
                                -- Shouldn't be graduated cards, period.
                                bad

                            ( New, New ) ->
                                good

                            ( New, _ ) ->
                                bad

                            ( _, New ) ->
                                good

                            ( BoxN box1 reviewed1, BoxN box2 reviewed2 ) ->
                                if overdueAmount box1 reviewed1 >= overdueAmount box2 reviewed2 then
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
                        List.filterMap (\{ index, queueDetails } -> Maybe.map (\c -> ( c, queueDetails )) <| Array.get index deck.cards) <| getDueCardIndicesWithDetails time deck

                    checkQueue c =
                        case c.srsData of
                            BoxN boxNumber lastSeen ->
                                InBox
                                    { lastSeen = lastSeen
                                    , boxNumber = boxNumber
                                    }

                            Graduated ->
                                GraduatedCard

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
