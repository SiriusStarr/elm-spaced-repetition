# SpacedRepetition

[![Build Status](https://github.com/SiriusStarr/elm-spaced-repetition/actions/workflows/ci.yml/badge.svg)](https://github.com/SiriusStarr/elm-spaced-repetition/actions/workflows/ci.yml)

## Introduction

This package provides everything necessary to create spaced repetition software
using a number of different popular algorithms.  Information on the various
algorithms available and the general structure of the package is provided below.

## Algorithms

This package provides four different algorithms, three of which are based on the
SM-2 scheduling algorithm.  If you're **not sure which to use**, the algorithm
provided by **`SMTwoAnki`** (which reproduces the scheduling behavior of the popular
F/OSS flashcard software Anki) is **most likely the "best" option.**

### Leitner ([`SpacedRepetition.Leitner`](https://package.elm-lang.org/packages/SiriusStarr/elm-spaced-repetition/2.0.1/SpacedRepetition-Leitner))

The Leitner system was proposed by Sebastian Leitner in the early 1970s and was
originally intended for use with physical (paper) flashcards.  For the basics
about this algorithm, please refer to the [following description](https://en.wikipedia.org/wiki/Leitner_system)
on Wikipedia.

In general, the Leitner system is much more simple than the other options
provided (given that it was meant to be workable by hand) and should generally
be considered deprecated in favor of the more advanced SM-2 based algorithms.

### SM-2 ([`SpacedRepetition.SMTwo`](https://package.elm-lang.org/packages/SiriusStarr/elm-spaced-repetition/2.0.1/SpacedRepetition-SMTwo))

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

### SM2+ ([`SpaceRepetition.SMTwoPlus`](https://package.elm-lang.org/packages/SiriusStarr/elm-spaced-repetition/2.0.1/SpacedRepetition-SMTwoPlus))

The SM2+ algorithm was proposed by "BlueRaja" as an improvement of the SM-2
algorithm.  For details about the SM2+ algorithm and its purported advantages
over the SM-2 algorithm, please refer to the [following blog post](http://www.blueraja.com/blog/477/a-better-spaced-repetition-learning-algorithm-sm2).

**It should be noted that this algorithm produces seemingly illogical
behavior,** namely that more incorrect answers result in longer intervals than
less incorrect answers.  In general, this algorithm has serious flaws as
presented in its reference implementation and its superiority to the SM-2
algorithm is dubious at best.  Nevertheless, it is implemented here as it is
popular and often-cited online.

### Anki ([`SpaceRepetition.SMTwoAnki`](https://package.elm-lang.org/packages/SiriusStarr/elm-spaced-repetition/2.0.1/SpacedRepetition-SMTwoAnki))

The algorithm used by the popular F/OSS program Anki, this algorithm is a
heavily-modified version of the SM-2 algorithm.  For details about Anki's
algorithm, please refer to [the following section of its manual](https://faqs.ankiweb.net/what-spaced-repetition-algorithm.html).

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

### `QueueDetails`, `getCardDetails`, `getDueCardIndicesWithDetails`

If you need information about the SRS status of a card (e.g. when it was last
reviewed, whether it's new, etc.), such information may be found in the
`QueueDetails` of a module.  `QueueDetails` may be obtained from a single `Card`
with `getCardDetails` or along with the indices of due cards with
`getDueCardIndicesWithDetails`.

### Miscellaneous

The various algorithms provide additional functions/types as necessary for their
individual implementations.  Refer to their documentation for specifics.

## Changelog

* `2.1.0`
  * Actual JSON encoding has **not** changed, so this is compatible with JSON
    generated by `2.0.1`, but validation is more strict on things that should
    be non-negative (which would not normally have been written by `2.0.1`, so
    it should not cause issues).
  * 🐛 **Bugfix:**  For all algorithms, equivalently-due cards would appear in
    reverse input order. This shuffles their order instead, to prevent the
    same ordering from occurring repeatedly.
  * `SpacedRepetition.Leitner`
    * 🏷️ `NumberOfBoxes` is now exposed (but still opaque), so you may write type
      signatures with it.
    * 📝 Note that cards will be graduated after answering (even with `Pass`) if
      they're in an invalid box beyond `NumberOfBoxes`.  This was always the
      case, but it's mentioned in the documentation now.
    * 🐛 **Bugfix:**  Enforce `SpacingFunctions` returning an interval of at least
      1 day; this was always the case per documentation and you definitely
      couldn't cause problems by returning zero prior to this version (shh...).
    * ⚡️ Tail-call optimized `fibonacciSpacing`, so you can have intervals of 10^38
      years for your 200th box if you're an eternal but not omniscient being.
  * `SpacedRepetition.SMTwoAnki`
    * 🐛 **Bugfix:** Ensure that cards always graduate from being "lapsed"
      *regardless of the answer if there are no lapse steps*.  This was
      the behavior specified in the documentation of `lapseSteps`, but it wasn't
      actually happening.  Now, answering `Hard` or `Again` on a "lapsed" card
      will return it to the review queue if there are no lapse steps.
      * Old behavior: With no lapse steps, failing a review card will lapse it,
        making it immediately due for review.  Answering `Hard` or `Again` will
        leave it immediately due.  Answering `Good` or `Easy` will return it to
        the review queue.
      * New behavior: With no lapse steps, failing a review card will lapse it,
        making it immediately due for review.  Answering the card with *any*
        answer will return it to the review queue.
    * 🐛 **Bugfix:** Ensure that cards always graduate from learning
      *regardless of the answer if there are no learning steps*.  This was the
      behavior specified in the documentation of `newSteps`, but it wasn't
      actually happening.  Now, answering `Hard` or `Again` on a learning card
      will graduate it to the review queue if there are no learning steps.
      Unlike the case with lapses, however, this bug should have been quite rare
      in practice, as the only way to end up with cards in the learning queue
      with no learning steps would be to change the settings of a deck that had
      already been partially studied to remove previously-extant `newSteps`.
    * 🚸 `getLeeches` now returns cards with the same number of lapses in
      order of their appearance in the input deck.
    * 🏷️ `TimeInterval`, `Days`, and `Minutes` are now exposed (but still
      opaque), so you may write type signatures with them.
    * 📝 Fix broken links to Anki documentation, since the URLs moved.
    * 🩹 Fix bug in non-exposed function.  This bug could not have actually
      caused erroneous behavior in any exposed functions, but it might have
      going forwards had its output been used for something else.
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
