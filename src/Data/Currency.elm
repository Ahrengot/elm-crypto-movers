module Data.Currency exposing (..)

import Json.Decode as Decode exposing (Decoder)


type alias Currency =
    { id : Int
    , symbol : String
    , name : String
    , rank : Int
    , websiteSlug : String
    , circulatingSupply : Float
    , totalSupply : Float
    , maxSupply : Maybe Float
    }


decode : Decoder Currency
decode =
    Decode.map8 Currency
        (Decode.field "id" Decode.int)
        (Decode.field "symbol" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "rank" Decode.int)
        (Decode.field "website_slug" Decode.string)
        (Decode.field "circulating_supply" Decode.float)
        (Decode.field "total_supply" Decode.float)
        (Decode.field "max_supply" (Decode.maybe Decode.float))
