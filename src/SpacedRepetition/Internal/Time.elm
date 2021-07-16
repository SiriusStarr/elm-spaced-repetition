module SpacedRepetition.Internal.Time exposing (decode, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


{-| Decode a `Time.Posix` from JSON, where the value has been stored as seconds,
since that loss of precision is acceptable for our purposes.
-}
decode : Decoder Time.Posix
decode =
    Decode.map (\t -> Time.millisToPosix <| t * 1000) Decode.int


{-| Encode a `Time.Posix` as JSON, storing the value as seconds, since that loss
of precision is acceptable for our purposes.
-}
encode : Time.Posix -> Encode.Value
encode t =
    Encode.int <| Time.posixToMillis t // 1000
