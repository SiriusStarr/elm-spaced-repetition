module SpacedRepetition.Internal.SMTwoAnki exposing (Days(..), Ease, Lapses, Minutes(..), QueueStatus(..), Step, TimeInterval(..), createEase, createLapses, createStep, createTimeIntervalInDays, createTimeIntervalInMinutes, easeToFloat, lapsesToInt, minutesToDayInterval, stepToInt, timeIntervalToDays, timeIntervalToMinutes)

import Time


type Minutes
    = Minutes


type Days
    = Days


type Ease
    = Ease Float


type TimeInterval a
    = TimeInterval Int -- In minutes


type QueueStatus
    = New
    | Learning Step LastReviewed
    | Review Ease (TimeInterval Days) LastReviewed Lapses
    | Lapsed Ease Step (TimeInterval Days) LastReviewed Lapses


type Step
    = Step Int


type Lapses
    = Lapses Int


type alias LastReviewed =
    Time.Posix


createEase : Float -> Ease
createEase f =
    Ease <| max 1.3 f


easeToFloat : Ease -> Float
easeToFloat ease =
    case ease of
        Ease f ->
            f


createStep : Int -> Step
createStep i =
    Step <| max 0 i


stepToInt : Step -> Int
stepToInt step =
    case step of
        Step i ->
            i


createLapses : Int -> Lapses
createLapses i =
    Lapses <| max 0 i


lapsesToInt : Lapses -> Int
lapsesToInt lapses =
    case lapses of
        Lapses i ->
            i


timeIntervalToMinutes : TimeInterval a -> Int
timeIntervalToMinutes interval =
    case interval of
        TimeInterval i ->
            i


timeIntervalToDays : TimeInterval Days -> Int
timeIntervalToDays interval =
    case interval of
        TimeInterval i ->
            i // 1440


minutesToDayInterval : Int -> TimeInterval Days
minutesToDayInterval i =
    TimeInterval << max 1440 <| 1440 * (i // 1440)


createTimeIntervalInMinutes : Int -> TimeInterval Minutes
createTimeIntervalInMinutes i =
    -- This magic number is max int; truncate does not play well with larger values.
    TimeInterval <| clamp 1 2147483647 i


createTimeIntervalInDays : Int -> TimeInterval Days
createTimeIntervalInDays i =
    -- This magic number is max int; truncate does not play well with larger values.
    TimeInterval << clamp 1440 2147483647 <| i * 1440
