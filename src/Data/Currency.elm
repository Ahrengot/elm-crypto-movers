module Data.Currency exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline exposing (required)


type alias Currency =
    { id : Int
    , symbol : String
    , name : String
    , rank : Int
    , websiteSlug : String
    , circulatingSupply : Float
    , quotes : CurrencyQuote
    }


type alias CurrencyQuote =
    { price : Float
    , volume24h : Float
    , marketCap : Float
    , percentChange1h : Float
    , percentChange24h : Float
    , percentChange7d : Float
    }


decoder : Decoder Currency
decoder =
    DecodePipeline.decode Currency
        |> required "id" Decode.int
        |> required "symbol" Decode.string
        |> required "name" Decode.string
        |> required "rank" Decode.int
        |> required "website_slug" Decode.string
        |> required "circulating_supply" Decode.float
        |> DecodePipeline.custom (Decode.field "quotes" quoteDecoder)


quoteDecoder : Decoder CurrencyQuote
quoteDecoder =
    Decode.map6 CurrencyQuote
        (Decode.at [ "USD", "price" ] Decode.float)
        (Decode.at [ "USD", "volume_24h" ] Decode.float)
        (Decode.at [ "USD", "market_cap" ] Decode.float)
        (Decode.at [ "USD", "percent_change_1h" ] (Decode.oneOf [ Decode.null 0, Decode.float ]))
        (Decode.at [ "USD", "percent_change_24h" ] (Decode.oneOf [ Decode.null 0, Decode.float ]))
        (Decode.at [ "USD", "percent_change_7d" ] (Decode.oneOf [ Decode.null 0, Decode.float ]))
