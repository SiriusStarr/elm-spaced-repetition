# SpacedRepetition

[![Build Status](https://travis-ci.com/SiriusStarr/elm-spaced-repetition.svg?branch=master)](https://travis-ci.com/SiriusStarr/elm-spaced-repetition)

## Introduction

This package provides everything necessary to create spaced repetition software
using a number of different popular algorithms.  Information on the various
algorithms available and the general structure of the package is provided below.

## Algorithms

This package provides four different algorithms, three of which are based on the
SM-2 scheduling algorithm.  If you're **not sure which to use**, the algorithm
provided by **`SMTwoAnki`** (which reproduces the scheduling behavior of the popular
F/OSS flashcard software Anki) is **most likely the "best" option.**

### Leitner (`SpacedRepetition.Leitner`)

The Leitner system was proposed by Sebastian Leitner in the early 1970s and was
originally intended for use with physical (paper) flashcards.  For the basics
about this algorithm, please refer to the [following description](https://en.wikipedia.org/wiki/Leitner_system)
on Wikipedia.

In general, the Leitner system is much more simple than the other options
provided (given that it was meant to be workable by hand) and should generally
be considered deprecated in favor of the more advanced SM-2 based algorithms.

### SM-2 (`SpacedRepetition.SMTwo`)

The SM-2 algorithm was one of the earliest computerized implementations of a
spaced repetition algorithm (created in 1988 by Piotr Wozniak) and has been
released for free public use when accompanied by the following notice:

**Algorithm SM-2, (C) Copyright SuperMemo World, 1991.**

* <http://www.supermemo.com>
* <http://www.supermemo.eu>

For details about this algorithm, please refer to the [following description](https://www.supermemo.com/en/archives1990-2015/english/ol/sm2),
written by its creator.

The SM-2 algorithm is robust, if more rudimentary than Anki's variant.  Still,
it may be useful for those desiring a simple system without settings that can
still adapt to individual card difficulties (unlike the Leitner system).

### SM2+ (`SpaceRepetition.SMTwoPlus`)

The SM2+ algorithm was proposed by "BlueRaja" as an improvement of the SM-2
algorithm.  For details about the SM2+ algorithm and its purported advantages
over the SM-2 algorithm, please refer to the [following blog post](http://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2).

**It should be noted that this algorithm produces seemingly illogical
behavior,** namely that more incorrect answers result in longer intervals than
less incorrect answers.  In general, this algorithm has serious flaws as
presented in its reference implementation and its superiority to the SM-2
algorithm is dubious at best.  Nevertheless, it is implemented here as it is
popular and often-cited online.

### Anki (`SpaceRepetition.SMTwoAnki`)

The algorithm used by the popular F/OSS program Anki, this algorithm is a
heavily-modified version of the SM-2 algorithm.  For details about Anki's
algorithm, please refer to [the following section of its manual](https://apps.ankiweb.net/docs/manual.html#what-spaced-repetition-algorithm-does-anki-use).

This is by far the most powerful and flexible algorithm provided in this package
and should be considered the "default" for most users.

## General Use

The following functions/types are provided by every algorithm:

### `Card` and `Deck`

The building blocks of this package are `Card`s and `Deck`s. In simple terms, a
`Card` may be thought of as a single flashcard and a `Deck` as a list or
collection of `Card`s.  `Card` is always defined in terms of an extensible
record and contains only the data necessary for scheduling, so that the user of
this package may add whatever fields they find necessary for actually holding
data on the card.

### `SRSData` and `newSRSData`

`SRSData` contains all information necessary for scheduling a card.  In all
cases, a `Card` may be created by use of the `newSRSData` function, as in the
following example:

```elm
type alias MyFlashcard =
    Card { prompt : String, answer : String }

myFlashcard : MyFlashcard
myFlashcard =
    { prompt = "SYN"
    , answer = "SYN-ACK"
    , srsData = newSRSData
    }
```

### `encoderSRSData` and `decoderSRSData`

All algorithms provide Json encoders and decoders for SRS data, which may be
utilized as follows:

```elm
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
```

### `answerCardInDeck` and `answerCard`

When a card is presented to the user and answered, `answerCardInDeck` should be
called.  It always takes the current time (in the `Time.Posix` format returned
by the `now` task of the core `Time` module), some sort of answer or performance
(the `Answer` type for all algorithms except SM2+), the index of the card in the
`Deck`, and the `Deck` itself. It returns the updated `Deck`. Use this function
if you simply want to store a `Deck` and not worry about updating it manually
(which is most likely what you want). Otherwise, use `answerCard` to handle
updating the `Deck` manually.

### `getDueCardIndices`

`getDueCardIndices` takes the current time (in the `Time.Posix` format returned
by the `now` task of the core `Time` module) and a `Deck` and returns the
indices of the subset of the `Deck` that is due for review.  The sorting of the
results varies with the algorithm.

### Miscellaneous

The various algorithms provide additional functions/types as necessary for their
individual implementations.  Refer to their documentation for specifics.

## Changelog

* `2.0.1` -- 🐛 Fixed a bug in `SpacedRepetition.SMTwoAnki` that caused the
  extra interval from studying an overdue card to not count with `Good` answers.
  Per the algorithm, half of the overdue amount should be included in
  calculating the new interval with a `Good` answer.
* `2.0.0` -- Added `getDueCardIndicesWithDetails` and `getCardDetails` to all
  modules, allowing one to get information about e.g. what stage of learning a
  card is in so that it might be displayed differently.   This was a
  **breaking change** because the return type of
  `SpacedRepetition.SMTwoAnki.getDueCardIndices` changed to no longer return
  leech status (use `getDueCardIndicesWithDetails` or `getCardDetails` to get
  leech status).
* `1.1.0` -- Added a JSON encoder/decoder for
  `SpacedRepetition.SMTwoAnki.AnkiSettings`
* `1.0.0` -- Initial release.
