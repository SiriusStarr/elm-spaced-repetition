module Util exposing (boundedGreaterThan, boundedLessThan, fuzzNatural, fuzzTime)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, intRange)
import Random
import SpacedRepetition.Internal.Natural as Natural exposing (Natural)
import Time


{-| Given a lower bound and two numbers, ensure that the latter is less than the
former or bounded at the lower bound.
-}
boundedLessThan : Float -> Float -> Float -> Expectation
boundedLessThan bound old new =
    if old <= bound then
        Expect.within (Absolute 0.000000001) bound new

    else
        Expect.lessThan old new


{-| Given an upper bound and two numbers, ensure that the latter is greater than
the former or bounded at the upper bound.
-}
boundedGreaterThan : Float -> Float -> Float -> Expectation
boundedGreaterThan bound old new =
    if old >= bound then
        Expect.within (Absolute 0.000000001) bound new

    else
        Expect.greaterThan old new


{-| Fuzz a natural number.
-}
fuzzNatural : Fuzzer Natural
fuzzNatural =
    intRange 0 1000
        |> Fuzz.map Natural.fromInt
        |> Fuzz.map (Maybe.withDefault Natural.nil)


{-| Fuzz a time.
-}
fuzzTime : Fuzzer Time.Posix
fuzzTime =
    Fuzz.map (\i -> Time.millisToPosix (1000 * i)) (intRange 1 Random.maxInt)
