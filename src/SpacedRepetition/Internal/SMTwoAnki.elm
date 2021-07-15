module SpacedRepetition.Internal.SMTwoAnki exposing
    ( Days
    , Ease
    , Minutes
    , QueueStatus(..)
    , TimeInterval
    , createEase
    , decodeDayInterval
    , decodeEase
    , decodeTime
    , easeToFloat
    , encodeDayInterval
    , encodeEase
    , encodeMinuteInterval
    , encodeTime
    , minutesToDayInterval
    , timeIntervalFromDays
    , timeIntervalFromMinutes
    , timeIntervalToDays
    , timeIntervalToMinutes
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import SpacedRepetition.Internal.Natural exposing (Natural)
import Time


{-| Phantom type for a `TimeInterval` in minutes.
-}
type Minutes
    = Minutes


{-| Phantom type for a `TimeInterval` consisting of a round number of days.
-}
type Days
    = Days


{-| Opaque type for the "ease" of a card, ensuring it is at least 1.3.
-}
type Ease
    = Ease Float


{-| A type for a positive, non-zero duration of time (stored internally in
minutes). Uses the phantom types `Minutes` and `Days` to limit time intervals to
a round number of a duration.
-}
type TimeInterval a
    = TimeInterval Int -- In minutes


{-| The status of a card:

  - `New` -- Never before encountered.
  - `Learning` -- In the early stages of learning the card, going through the initial short-interval steps.
  - `Review` -- A card in the maintenance stage of reviewing.
  - `Lapsed` -- A card that one has recently failed, going through the short-interval steps of refreshing one's memory of the card.

-}
type QueueStatus
    = Lapsed
        { ease : Ease
        , lapses : Natural
        , lastReviewed : Time.Posix
        , oldInterval : TimeInterval Days
        , step : Natural
        }
    | Learning { lastReviewed : Time.Posix, step : Natural }
    | New
    | Review
        { ease : Ease
        , interval : TimeInterval Days
        , lapses : Natural
        , lastReviewed : Time.Posix
        }


{-| Convert a `Float` into an `Ease`, ensuring it is at least 1.3.
-}
createEase : Float -> Ease
createEase f =
    Ease <| max 1.3 f


{-| Unpack an `Ease` into a `Float`.
-}
easeToFloat : Ease -> Float
easeToFloat (Ease f) =
    f


{-| Decode an `Ease` from JSON, ensuring it is at least 1.3.
-}
decodeEase : Decoder Ease
decodeEase =
    Decode.map createEase Decode.float


{-| Encode an `Ease` to JSON.
-}
encodeEase : Ease -> Encode.Value
encodeEase e =
    Encode.float <| easeToFloat e


{-| Unpack a `TimeInterval` as minutes.
-}
timeIntervalToMinutes : TimeInterval a -> Int
timeIntervalToMinutes (TimeInterval i) =
    i


{-| Unpack a `TimeInterval` as days.
-}
timeIntervalToDays : TimeInterval Days -> Int
timeIntervalToDays (TimeInterval i) =
    i // 1440


{-| Convert a number of minutes to a round, positive number of days.
-}
minutesToDayInterval : Int -> TimeInterval Days
minutesToDayInterval i =
    TimeInterval << max 1440 <| 1440 * (i // 1440)


{-| Convert a number of minutes to a `TimeInterval`, ensuring it is at least 1
and not over MaxInt.
-}
timeIntervalFromMinutes : Int -> TimeInterval Minutes
timeIntervalFromMinutes i =
    -- This magic number is max int; truncate does not play well with larger values.
    TimeInterval <| clamp 1 2147483647 i


{-| Convert a number of days to a `TimeInterval`, ensuring it is at least 1 day
and not over MaxInt minutes.
-}
timeIntervalFromDays : Int -> TimeInterval Days
timeIntervalFromDays i =
    -- This magic number is max int; truncate does not play well with larger values.
    TimeInterval << clamp 1440 2147483647 <| i * 1440


{-| Decode a `TimeInterval` in days from JSON (where it has been stored as
minutes).
-}
decodeDayInterval : Decoder (TimeInterval Days)
decodeDayInterval =
    Decode.map minutesToDayInterval Decode.int


{-| Encode a `TimeInterval` in days to JSON (where it will be stored as
minutes).
-}
encodeDayInterval : TimeInterval Days -> Encode.Value
encodeDayInterval interval =
    Encode.int <| timeIntervalToMinutes interval


{-| Encode a `TimeInterval` in minutes to JSON (where it will be stored as
minutes).
-}
encodeMinuteInterval : TimeInterval Minutes -> Encode.Value
encodeMinuteInterval interval =
    Encode.int <| timeIntervalToMinutes interval


{-| Decode a `Time.Posix` from JSON, where the value has been stored as seconds,
since that loss of precision is acceptable for our purposes.
-}
decodeTime : Decoder Time.Posix
decodeTime =
    Decode.map (\t -> Time.millisToPosix <| t * 1000) Decode.int


{-| Encode a `Time.Posix` as JSON, storing the value as seconds, since that loss
of precision is acceptable for our purposes.
-}
encodeTime : Time.Posix -> Encode.Value
encodeTime t =
    Encode.int <| Time.posixToMillis t // 1000
