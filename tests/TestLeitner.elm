module TestLeitner exposing (suite)

import Array exposing (Array)
import Array.Extra as ArrayX
import Basics.Extra exposing (flip, safeDivide)
import Expect exposing (Expectation)
import Fuzz
    exposing
        ( Fuzzer
        , int
        , intRange
        )
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListX
import Random
import SpacedRepetition.Internal.Leitner exposing (Box(..), highestBoxIndex)
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import SpacedRepetition.Leitner
    exposing
        ( Answer(..)
        , Card
        , LeitnerSettings
        , NumberOfBoxes
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
import Util exposing (fuzzNatural, fuzzTime)


{-| Tests for Leitner algorithm.
-}
suite : Test
suite =
    describe "Leitner"
        [ suiteAnswerCard
        , suiteAnswerCardInDeck
        , suiteGetDueCardIndices
        , suiteGetDueCardIndicesWithDetails
        , suiteJson
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
                let
                    { srsData } =
                        answerCard time answer settings card
                in
                case srsData of
                    New ->
                        Expect.fail "Card was still 'new' after answering"

                    BoxN { lastReviewed } ->
                        Expect.equal time lastReviewed

                    Graduated ->
                        Expect.pass
        , fuzz2 fuzzResponse fuzzCard "Box should never be < 0 or greater than max boxes" <|
            \( time, answer, settings ) card ->
                let
                    { srsData } =
                        answerCard time answer settings card
                in
                case srsData of
                    BoxN { box } ->
                        Natural.toInt box
                            |> Expect.all
                                [ Expect.atLeast 0
                                , highestBoxIndex settings.numBoxes
                                    |> Natural.toInt
                                    |> Expect.atMost
                                ]

                    _ ->
                        Expect.pass
        , fuzz2 fuzzResponse fuzzCard "Box should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\data ->
                            let
                                expectFirstBox : Expectation
                                expectFirstBox =
                                    Expect.equal Natural.nil newBox

                                expectMoved : Int -> Expectation
                                expectMoved i =
                                    Expect.equal
                                        (Natural.toInt oldBox
                                            + i
                                            -- Graduate cards that are over max boxes
                                            |> min (1 + Natural.toInt maxBox)
                                            |> Natural.fromInt
                                            |> Maybe.withDefault Natural.nil
                                        )
                                        newBox

                                oldBox : Natural
                                oldBox =
                                    case card.srsData of
                                        New ->
                                            Natural.nil

                                        BoxN { box } ->
                                            box

                                        Graduated ->
                                            Natural.succ maxBox

                                maxBox : Natural
                                maxBox =
                                    highestBoxIndex settings.numBoxes

                                newBox : Natural
                                newBox =
                                    case data of
                                        New ->
                                            Natural.nil

                                        BoxN { box } ->
                                            box

                                        Graduated ->
                                            Natural.succ maxBox
                            in
                            case ( answer, settings.onIncorrect ) of
                                ( Correct, _ ) ->
                                    expectMoved 1

                                ( Incorrect, BackOneBox ) ->
                                    expectMoved -1

                                ( Incorrect, BackToStart ) ->
                                    expectFirstBox

                                ( Pass, _ ) ->
                                    expectMoved 0

                                ( MoveBoxes i, _ ) ->
                                    expectMoved i

                                ( BackToFirstBox, _ ) ->
                                    expectFirstBox
                       )
        , fuzz2 fuzzResponse fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .unrelatedField
                    |> Expect.equal card.unrelatedField
        ]


{-| Fuzz a `SRSData` for a card with other fields.
-}
fuzzExtendedCard : Fuzzer { unrelatedField : Int, srsData : SRSData }
fuzzExtendedCard =
    Fuzz.map2 (\d i -> { unrelatedField = i, srsData = d }) fuzzSRSData int


{-| Fuzz a full user response, with settings, time, and answer.
-}
fuzzResponse : Fuzzer ( Time.Posix, Answer, LeitnerSettings )
fuzzResponse =
    Fuzz.map3 (\t a s -> ( t, a, s )) fuzzTime fuzzAnswer fuzzSettings


{-| Tests for `answerCardInDeck`.
-}
suiteAnswerCardInDeck : Test
suiteAnswerCardInDeck =
    describe "answerCardInDeck"
        [ fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Settings shouldn't be touched" <|
            \( time, answer ) deck i ->
                answerCardInDeck time answer i deck
                    |> .settings
                    |> Expect.equal deck.settings
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Cards other than index should be unaffected" <|
            \( time, answer ) deck index ->
                answerCardInDeck time answer index deck
                    |> .cards
                    |> ArrayX.zip deck.cards
                    |> ArrayX.indexedMapToList
                        (\i ( c1, c2 ) ->
                            i == index || c1 == c2
                        )
                    |> List.all identity
                    |> Expect.true "Only updated card should change in deck"
        , fuzz3 (Fuzz.tuple ( fuzzTime, fuzzAnswer )) fuzzDeck int "Answering card by index should be the same as answering independently" <|
            \( time, answer ) deck i ->
                let
                    updatedCard : Maybe (Card {})
                    updatedCard =
                        Array.get i deck.cards
                            |> Maybe.map (answerCard time answer deck.settings)

                    updatedCardInDeck : Maybe (Card {})
                    updatedCardInDeck =
                        answerCardInDeck time answer i deck
                            |> .cards
                            |> Array.get i
                in
                Expect.equal updatedCard updatedCardInDeck
        ]


{-| Test `getDueCardIndices`.
-}
suiteGetDueCardIndices : Test
suiteGetDueCardIndices =
    describe "getDue"
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
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain Graduated cards" <|
            \deck time ->
                List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)
                    |> ListX.count ((==) Graduated << .srsData)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should contain all cards that are due" <|
            \deck time ->
                let
                    due : List Int
                    due =
                        getDueCardIndices time deck

                    isDue : { srsData : Box } -> Bool
                    isDue c =
                        case c.srsData of
                            New ->
                                True

                            BoxN { box, lastReviewed } ->
                                overdueAmount box lastReviewed >= 1

                            Graduated ->
                                False

                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount box reviewed =
                        safeDivide (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5)
                            (toFloat (deck.settings.boxSpacing <| Natural.toInt box))
                            |> Maybe.withDefault 1
                in
                Array.toIndexedList deck.cards
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count (isDue << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain cards that are not due" <|
            \deck time ->
                let
                    isDue : { srsData : Box } -> Bool
                    isDue c =
                        case c.srsData of
                            New ->
                                True

                            BoxN { box, lastReviewed } ->
                                overdueAmount box lastReviewed >= 1

                            Graduated ->
                                False

                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount box reviewed =
                        safeDivide (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5)
                            (toFloat (deck.settings.boxSpacing <| Natural.toInt box))
                            |> Maybe.withDefault 1
                in
                List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)
                    |> ListX.count (not << isDue)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should be sorted." <|
            \deck time ->
                let
                    dueDeck : List { srsData : SRSData }
                    dueDeck =
                        List.filterMap (\i -> Array.get i deck.cards) (getDueCardIndices time deck)

                    firstCard : { srsData : Box }
                    firstCard =
                        Maybe.withDefault { srsData = New } <| List.head dueDeck

                    step : { srsData : Box } -> ( { srsData : Box }, Bool ) -> ( { srsData : Box }, Bool )
                    step nextCard ( lastCard, goodSort ) =
                        let
                            bad : ( { srsData : Box }, Bool )
                            bad =
                                ( nextCard, False )

                            good : ( { srsData : Box }, Bool )
                            good =
                                ( nextCard, goodSort )
                        in
                        case ( lastCard.srsData, nextCard.srsData ) of
                            ( New, New ) ->
                                good

                            ( New, _ ) ->
                                bad

                            ( BoxN b1, BoxN b2 ) ->
                                if overdueAmount b1 >= overdueAmount b2 then
                                    good

                                else
                                    bad

                            ( Graduated, _ ) ->
                                -- Shouldn't be graduated cards, period.
                                bad

                            ( _, New ) ->
                                good

                            ( _, Graduated ) ->
                                -- Shouldn't be graduated cards, period.
                                bad

                    overdueAmount : { box : Natural, lastReviewed : Time.Posix } -> Float
                    overdueAmount { box, lastReviewed } =
                        safeDivide (toFloat (diff Hour Time.utc lastReviewed time) / 24 + 0.5)
                            (toFloat (deck.settings.boxSpacing <| Natural.toInt box))
                            |> Maybe.withDefault 1
                in
                List.drop 1 dueDeck
                    |> List.foldl step ( firstCard, True )
                    |> Tuple.second
                    |> Expect.true "Expected a sorted deck"
        ]


{-| Tests for `getDueCardIndicesWithDetails`.
-}
suiteGetDueCardIndicesWithDetails : Test
suiteGetDueCardIndicesWithDetails =
    describe "getDueCardIndicesWithDetails"
        [ fuzz2 fuzzDeck fuzzTime "Queue status should be correct" <|
            \deck time ->
                let
                    checkQueue : { srsData : Box } -> QueueDetails
                    checkQueue c =
                        case c.srsData of
                            New ->
                                NewCard

                            BoxN { box, lastReviewed } ->
                                InBox
                                    { boxNumber = Natural.toInt box
                                    , lastReviewed = lastReviewed
                                    }

                            Graduated ->
                                GraduatedCard
                in
                getDueCardIndicesWithDetails time deck
                    |> List.filterMap
                        (\{ index, queueDetails } ->
                            Array.get index deck.cards
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


{-| Fuzz a review answer.
-}
fuzzAnswer : Fuzzer Answer
fuzzAnswer =
    Fuzz.oneOf
        [ Fuzz.constant Correct
        , Fuzz.constant Incorrect
        , Fuzz.constant Pass
        , Fuzz.map MoveBoxes int
        , Fuzz.constant BackToFirstBox
        ]


{-| Fuzz a `SRSData` for a card.
-}
fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    Fuzz.map (\d -> { srsData = d }) fuzzSRSData


{-| Fuzz a `Deck` of cards.
-}
fuzzDeck :
    Fuzzer
        { cards : Array { srsData : SRSData }
        , settings : LeitnerSettings
        }
fuzzDeck =
    Fuzz.map2 (\c s -> { cards = c, settings = s })
        (Fuzz.array fuzzCard)
        fuzzSettings


{-| Fuzz data for a card.
-}
fuzzSRSData : Fuzzer SRSData
fuzzSRSData =
    Fuzz.oneOf
        [ Fuzz.constant New
        , Fuzz.map2
            (\box lastReviewed ->
                BoxN
                    { box = box
                    , lastReviewed = lastReviewed
                    }
            )
            fuzzNatural
            fuzzTime
        , Fuzz.constant Graduated
        ]


{-| Fuzz `LeitnerSettings`.
-}
fuzzSettings : Fuzzer LeitnerSettings
fuzzSettings =
    Fuzz.map3
        (\boxSpacing numBoxes onIncorrect ->
            { onIncorrect = onIncorrect
            , boxSpacing = boxSpacing
            , numBoxes = numBoxes
            }
        )
        fuzzSpacing
        fuzzNumBoxes
        fuzzOnIncorrect


{-| Fuzz the number of boxes in the system.
-}
fuzzNumBoxes : Fuzzer NumberOfBoxes
fuzzNumBoxes =
    Fuzz.map numberOfBoxes <| intRange -1 Random.maxInt


{-| Fuzz `OnIncorrect` behavior.
-}
fuzzOnIncorrect : Fuzzer OnIncorrect
fuzzOnIncorrect =
    Fuzz.oneOf
        [ Fuzz.constant BackOneBox
        , Fuzz.constant BackToStart
        ]


{-| Fuzz a `SpacingFunction`.
-}
fuzzSpacing : Fuzzer SpacingFunction
fuzzSpacing =
    Fuzz.oneOf
        [ Fuzz.constant fibonacciSpacing
        , Fuzz.constant doubleSpacing
        , Fuzz.constant alternateSpacingFunction
        ]


{-| A stand-in for a user-provided spacing function.
-}
alternateSpacingFunction : SpacingFunction
alternateSpacingFunction i =
    i + 1
