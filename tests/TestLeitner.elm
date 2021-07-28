module TestLeitner exposing
    ( suiteAnswerCard
    , suiteAnswerCardInDeck
    , suiteGetDueCardIndices
    , suiteGetDueCardIndicesWithDetails
    , suiteJson
    )

import Array exposing (Array)
import Array.Extra as ArrayX
import Basics.Extra exposing (flip)
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


{-| Fuzz the number of boxes in the system.
-}
fuzzNumBoxes : Fuzzer NumberOfBoxes
fuzzNumBoxes =
    Fuzz.map numberOfBoxes <| intRange -1 Random.maxInt


{-| Fuzz `LeitnerSettings`.
-}
fuzzSettings : Fuzzer LeitnerSettings
fuzzSettings =
    Fuzz.map3
        (\boxSpacing numBoxes onIncorrect ->
            { boxSpacing = boxSpacing
            , numBoxes = numBoxes
            , onIncorrect = onIncorrect
            }
        )
        fuzzSpacing
        fuzzNumBoxes
        fuzzOnIncorrect


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


{-| Fuzz a `SRSData` for a card.
-}
fuzzCard : Fuzzer { srsData : SRSData }
fuzzCard =
    Fuzz.map (\d -> { srsData = d }) fuzzSRSData


{-| Fuzz a `SRSData` for a card with other fields.
-}
fuzzExtendedCard : Fuzzer { srsData : SRSData, unrelatedField : Int }
fuzzExtendedCard =
    Fuzz.map2 (\d i -> { srsData = d, unrelatedField = i }) fuzzSRSData int


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


{-| Fuzz a full user response, with settings, time, and answer.
-}
fuzzResponse : Fuzzer ( Time.Posix, Answer, LeitnerSettings )
fuzzResponse =
    Fuzz.map3 (\t a s -> ( t, a, s )) fuzzTime fuzzAnswer fuzzSettings


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
                    |> (\c ->
                            case c.srsData of
                                BoxN { lastReviewed } ->
                                    Expect.equal time lastReviewed

                                Graduated ->
                                    Expect.pass

                                New ->
                                    Expect.fail "Card was still 'new' after answering"
                       )
        , fuzz2 fuzzResponse fuzzCard "Box should never be < 0 or greater than max boxes" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> (\c ->
                            let
                                maxBox : Int
                                maxBox =
                                    Natural.toInt <| highestBoxIndex settings.numBoxes
                            in
                            case c.srsData of
                                BoxN { box } ->
                                    Natural.toInt box
                                        |> Expect.all
                                            [ Expect.atLeast 0
                                            , Expect.atMost maxBox
                                            ]

                                _ ->
                                    Expect.pass
                       )
        , fuzz2 fuzzResponse fuzzCard "Box should be updated" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .srsData
                    |> (\data ->
                            let
                                maxBox : Natural
                                maxBox =
                                    highestBoxIndex settings.numBoxes

                                oldBox : Natural
                                oldBox =
                                    case card.srsData of
                                        BoxN { box } ->
                                            box

                                        Graduated ->
                                            Natural.succ maxBox

                                        New ->
                                            Natural.nil

                                newBox : Natural
                                newBox =
                                    case data of
                                        BoxN { box } ->
                                            box

                                        Graduated ->
                                            Natural.succ maxBox

                                        New ->
                                            Natural.nil

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

                                expectFirstBox : Expectation
                                expectFirstBox =
                                    Expect.equal Natural.nil newBox
                            in
                            case ( answer, settings.onIncorrect ) of
                                ( BackToFirstBox, _ ) ->
                                    expectFirstBox

                                ( Correct, _ ) ->
                                    expectMoved 1

                                ( Incorrect, BackOneBox ) ->
                                    expectMoved -1

                                ( Incorrect, BackToStart ) ->
                                    expectFirstBox

                                ( MoveBoxes i, _ ) ->
                                    expectMoved i

                                ( Pass, _ ) ->
                                    expectMoved 0
                       )
        , fuzz2 fuzzResponse fuzzExtendedCard "Non-srs fields should never be changed by answering" <|
            \( time, answer, settings ) card ->
                answerCard time answer settings card
                    |> .unrelatedField
                    |> Expect.equal card.unrelatedField
        ]


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

                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount box reviewed =
                        (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5)
                            / toFloat (deck.settings.boxSpacing <| Natural.toInt box)

                    isDue : { srsData : Box } -> Bool
                    isDue c =
                        case c.srsData of
                            BoxN { box, lastReviewed } ->
                                overdueAmount box lastReviewed >= 1

                            Graduated ->
                                False

                            New ->
                                True
                in
                Array.toIndexedList deck.cards
                    |> List.filter (not << flip List.member due << Tuple.first)
                    |> ListX.count (isDue << Tuple.second)
                    |> Expect.equal 0
        , fuzz2 fuzzDeck fuzzTime "Due cards should not contain cards that are not due" <|
            \deck time ->
                let
                    overdueAmount : Natural -> Time.Posix -> Float
                    overdueAmount box reviewed =
                        (toFloat (diff Hour Time.utc reviewed time) / 24 + 0.5)
                            / toFloat (deck.settings.boxSpacing <| Natural.toInt box)

                    isDue : { srsData : Box } -> Bool
                    isDue c =
                        case c.srsData of
                            BoxN { box, lastReviewed } ->
                                overdueAmount box lastReviewed >= 1

                            Graduated ->
                                False

                            New ->
                                True
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

                    overdueAmount : { box : Natural, lastReviewed : Time.Posix } -> Float
                    overdueAmount { box, lastReviewed } =
                        (toFloat (diff Hour Time.utc lastReviewed time) / 24 + 0.5)
                            / toFloat (deck.settings.boxSpacing <| Natural.toInt box)

                    step : { srsData : Box } -> ( { srsData : Box }, Bool ) -> ( { srsData : Box }, Bool )
                    step nextCard ( lastCard, goodSort ) =
                        let
                            good : ( { srsData : Box }, Bool )
                            good =
                                ( nextCard, goodSort )

                            bad : ( { srsData : Box }, Bool )
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

                            ( BoxN b1, BoxN b2 ) ->
                                if overdueAmount b1 >= overdueAmount b2 then
                                    good

                                else
                                    bad
                in
                dueDeck
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
                            BoxN { box, lastReviewed } ->
                                InBox
                                    { boxNumber = Natural.toInt box
                                    , lastReviewed = lastReviewed
                                    }

                            Graduated ->
                                GraduatedCard

                            New ->
                                NewCard
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
