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


---- MODEL ----


type CurrencySort
    = Hourly
    | Daily
    | Weekly


type SortOrder
    = Ascending
    | Descending


type alias Model =
    { listRequest : RequestState (List Currency)
    , sorting : CurrencySort
    , sortOrder : SortOrder
    }


init : ( Model, Cmd Msg )
init =
    ( { listRequest = Loading
      , sorting = Hourly
      , sortOrder = Descending
      }
    , Request.Ticker.getTickers 50
        |> Http.send TickerListResponse
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
                | listRequest = Errored (HttpUtils.parseError error)
              }
            , Cmd.none
            )

        TickerListResponse (Ok currencyTuples) ->
            ( { model
                | listRequest = FinishedLoading (List.map Tuple.second currencyTuples)
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

        sortedCurrencies =
            List.sortBy (.quotes >> sortKey) currencies
    in
        case order of
            Ascending ->
                sortedCurrencies

            Descending ->
                List.reverse sortedCurrencies



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "display-3 text-center mb-4" ] [ text "💰 Movers 💰" ]
        , div [ class "container-fluid" ]
            [ case model.listRequest of
                Idle ->
                    text "Welcome!"

                Loading ->
                    text "Loading ticker data..."

                FinishedLoading currencies ->
                    viewTickersTable (sortCurrencies model.sorting model.sortOrder currencies)

                Errored errMsg ->
                    div [ class "alert alert-danger" ]
                        [ h4 [ class "alert-heading" ] [ text "Error loading tickers" ]
                        , div [] [ text errMsg ]
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
                , th [] [ text "price (USD)" ]
                , th [ sortCursorStyles, onClick (Sort Hourly) ] [ text "1h change" ]
                , th [ sortCursorStyles, onClick (Sort Daily) ] [ text "24h change" ]
                , th [ sortCursorStyles, onClick (Sort Weekly) ] [ text "7d change" ]
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
    in
        ( (toString currency.id)
        , tr []
            [ td []
                [ a
                    [ href ("https://coinmarketcap.com/currencies/" ++ currency.websiteSlug)
                    , target "_blank"
                    ]
                    [ text currency.name
                    ]
                ]
            , td [] [ text <| "$" ++ toString currency.quotes.price ]
            , td (valueAttrs currency.quotes.percentChange1h) [ text <| (toString currency.quotes.percentChange1h) ++ "%" ]
            , td (valueAttrs currency.quotes.percentChange24h) [ text <| (toString currency.quotes.percentChange24h) ++ "%" ]
            , td (valueAttrs currency.quotes.percentChange7d) [ text <| (toString currency.quotes.percentChange7d) ++ "%" ]
            ]
        )
