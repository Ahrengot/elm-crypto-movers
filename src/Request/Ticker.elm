module Request.Ticker exposing (..)

import Data.Currency exposing (Currency)
import Http
import HttpBuilder
import Json.Decode as Decode


{-| Get a list of n number of Currencies
-}
getTickers : Int -> Int -> Http.Request (List ( String, Currency ))
getTickers startAt limit =
    let
        decoder =
            Decode.field "data" (Decode.keyValuePairs Data.Currency.decoder)
    in
        HttpBuilder.get "https://api.coinmarketcap.com/v2/ticker/"
            |> HttpBuilder.withQueryParams
                [ ( "limit", toString limit )
                , ( "start", toString startAt )
                ]
            |> HttpBuilder.withExpectJson decoder
            |> HttpBuilder.toRequest
