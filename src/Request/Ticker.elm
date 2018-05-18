module Request.Ticker exposing (..)

import Data.Currency exposing (Currency)
import Http
import Json.Decode as Decode


{-| Get a list of n number of Currencies
-}
getTickers : Int -> Http.Request (List ( String, Currency ))
getTickers limit =
    let
        decoder =
            Decode.field "data" (Decode.keyValuePairs Data.Currency.decoder)
    in
        Http.get ("https://api.coinmarketcap.com/v2/ticker/?limit=" ++ (toString limit)) decoder
