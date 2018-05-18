module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Html.Events exposing (onClick)
import Data.Currency exposing (Currency)
import Data.Request exposing (RequestState(..))
import Request.Ticker
import Http
import HttpUtils
import Round
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)


---- MODEL ----


type CurrencySort
    = Hourly
    | Daily
    | Weekly
    | Price
    | Volume


type SortOrder
    = Ascending
    | Descending


type alias Model =
    { tickerRequest : RequestState (List Currency)
    , results : List Currency
    , sorting : CurrencySort
    , sortOrder : SortOrder
    }


init : ( Model, Cmd Msg )
init =
    ( { tickerRequest = Loading
      , results = []
      , sorting = Hourly
      , sortOrder = Descending
      }
    , Cmd.batch
        [ (Http.send TickerListResponse <| Request.Ticker.getTickers 0 100)
        , (Http.send TickerListResponse <| Request.Ticker.getTickers 101 100)
        , (Http.send TickerListResponse <| Request.Ticker.getTickers 201 100)
        , (Http.send TickerListResponse <| Request.Ticker.getTickers 301 100)
        , (Http.send TickerListResponse <| Request.Ticker.getTickers 401 100)
        ]
    )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }



---- UPDATE ----


type Msg
    = TickerListResponse (Result Http.Error (List ( String, Currency )))
    | Sort CurrencySort
    | Order SortOrder
    | ToggleOrder


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TickerListResponse (Err error) ->
            ( { model
                | tickerRequest = Errored (HttpUtils.parseError error)
              }
            , Cmd.none
            )

        TickerListResponse (Ok currencyTuples) ->
            let
                results =
                    (List.map Tuple.second currencyTuples)
            in
                ( { model
                    | tickerRequest = FinishedLoading results
                    , results = model.results ++ results
                  }
                , Cmd.none
                )

        Sort sorting ->
            let
                newModel =
                    { model | sorting = sorting }
            in
                -- Change order if clicking the same colum twice
                if model.sorting == sorting then
                    update ToggleOrder newModel
                else
                    newModel ! []

        ToggleOrder ->
            case model.sortOrder of
                Ascending ->
                    update (Order Descending) model

                Descending ->
                    update (Order Ascending) model

        Order ordering ->
            ( { model | sortOrder = ordering }, Cmd.none )



-- Utilities --


sortCurrencies : CurrencySort -> SortOrder -> List Currency -> List Currency
sortCurrencies sorting order currencies =
    let
        sortKey =
            case sorting of
                Hourly ->
                    .percentChange1h

                Daily ->
                    .percentChange24h

                Weekly ->
                    .percentChange7d

                Price ->
                    .price

                Volume ->
                    .volume24h

        sortedCurrencies =
            List.sortBy (.quotes >> sortKey) currencies
    in
        case order of
            Ascending ->
                sortedCurrencies

            Descending ->
                List.reverse sortedCurrencies


{-| Rounds dynamically based on value. E.g. 1000 won't have decimals
whereas 0.0054 will have 4 decimals
-}
dynamicRound : Float -> String
dynamicRound val =
    let
        decimals =
            if val > 100 then
                0
            else if val > 1 then
                2
            else if val > 0.5 then
                3
            else
                4
    in
        Round.round decimals val



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "display-3 text-center mb-4" ] [ text "💰 Movers 💰" ]
        , div [ class "container-fluid" ]
            [ case model.tickerRequest of
                Errored errMsg ->
                    div [ class "alert alert-danger" ]
                        [ h4 [ class "alert-heading" ] [ text "Error loading tickers" ]
                        , div [] [ text errMsg ]
                        ]

                _ ->
                    text ""
            , if List.isEmpty model.results then
                case model.tickerRequest of
                    Loading ->
                        div [ class "text-center" ]
                            [ text "Loading ticker data..."
                            ]

                    Errored errMsg ->
                        div [ class "alert alert-danger" ]
                            [ h4 [ class "alert-heading" ] [ text "Error loading tickers" ]
                            , div [] [ text errMsg ]
                            ]

                    _ ->
                        div [ class "text-center" ]
                            [ text "Huh? Not sure how we ended up in this state... But I need a lot of your cheapest, strongest alcohol."
                            ]
              else
                div []
                    [ h4 [ class "display-5 text-right text-muted mb-3" ] [ text <| "Found " ++ (toString (List.length model.results)) ++ " coins" ]
                    , viewTickersTable (sortCurrencies model.sorting model.sortOrder model.results)
                    ]
            ]
        ]


viewTickersTable : List Currency -> Html Msg
viewTickersTable currencyList =
    let
        sortCursorStyles =
            style [ ( "cursor", "ns-resize" ) ]
    in
        table [ class "table table-striped" ]
            [ thead []
                [ th [] [ text "Name" ]
                , th [ sortCursorStyles, onClick (Sort Price) ] [ text "price (USD)" ]
                , th [ sortCursorStyles, onClick (Sort Hourly) ] [ text "1h change" ]
                , th [ sortCursorStyles, onClick (Sort Daily) ] [ text "24h change" ]
                , th [ sortCursorStyles, onClick (Sort Weekly) ] [ text "7d change" ]
                , th [ sortCursorStyles, onClick (Sort Volume) ] [ text "24h volume" ]
                ]
            , Html.Keyed.node "tbody" [] (List.map viewTickerRow currencyList)
            ]


viewTickerRow : Currency -> ( String, Html Msg )
viewTickerRow currency =
    let
        colorIndicator : Float -> String
        colorIndicator val =
            if val == 0 then
                "#222"
            else if val > 0 then
                "#36f42f"
            else
                "#f42f32"

        valueAttrs : Float -> List (Html.Attribute Msg)
        valueAttrs val =
            [ style [ ( "color", colorIndicator val ) ] ]

        iconSrc =
            "https://s2.coinmarketcap.com/static/img/coins/32x32/" ++ (toString currency.id) ++ ".png"
    in
        ( (toString currency.id)
        , tr []
            [ td [ class "currency-name" ]
                [ a
                    [ href ("https://coinmarketcap.com/currencies/" ++ currency.websiteSlug)
                    , target "_blank"
                    ]
                    [ img [ src iconSrc ] []
                    , text currency.name
                    ]
                ]
            , td [] [ text <| "$" ++ (dynamicRound currency.quotes.price) ]
            , td (valueAttrs currency.quotes.percentChange1h) [ text <| (toString currency.quotes.percentChange1h) ++ "%" ]
            , td (valueAttrs currency.quotes.percentChange24h) [ text <| (toString currency.quotes.percentChange24h) ++ "%" ]
            , td (valueAttrs currency.quotes.percentChange7d) [ text <| (toString currency.quotes.percentChange7d) ++ "%" ]
            , td [] [ text <| "$" ++ (format { usLocale | decimals = 0 } currency.quotes.volume24h) ]
            ]
        )
