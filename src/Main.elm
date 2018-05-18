module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Keyed
import Html.Lazy exposing (lazy)
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
    , Cmd.batch (initialRequests 16)
    )


{-| Batches n number of ticker requests and returns a single command for them all
-}
initialRequests : Int -> List (Cmd Msg)
initialRequests numPages =
    List.range 0 (numPages - 1)
        |> List.map
            (\page ->
                let
                    startAt =
                        if page == 0 then
                            0
                        else
                            (page * 100) + 1
                in
                    (Http.send TickerListResponse (Request.Ticker.getTickers startAt 100))
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
                        -- Handled above...
                        text ""

                    FinishedLoading _ ->
                        div [ class "text-center" ]
                            [ text "Finished loading, got no errors, but also got no results... This is weird."
                            ]

                    Idle ->
                        div [ class "text-center" ]
                            [ text "Huh? Not sure how we could end up in this state... But I need a lot of your cheapest, strongest alcohol."
                            ]
              else
                div []
                    [ h4 [ class "display-5 text-right text-muted mb-3" ]
                        [ text <| "Loaded " ++ (format { usLocale | decimals = 0 } (toFloat (List.length model.results))) ++ " coins"
                        ]
                    , viewTickersTable (sortCurrencies model.sorting model.sortOrder model.results) model.sorting model.sortOrder
                    ]
            ]
        ]


viewTickersTable : List Currency -> CurrencySort -> SortOrder -> Html Msg
viewTickersTable currencyList currentSorting currentOrder =
    let
        sortableHeaders =
            [ ( "price", Price )
            , ( "1h change", Hourly )
            , ( "24h change", Daily )
            , ( "7d change", Weekly )
            , ( "24h volume", Volume )
            ]
                |> List.map
                    (\( label, sorter ) ->
                        th
                            [ style [ ( "cursor", "ns-resize" ) ]
                            , onClick (Sort sorter)
                            ]
                            [ if sorter == currentSorting then
                                case currentOrder of
                                    Ascending ->
                                        text (label ++ " 📉")

                                    Descending ->
                                        text (label ++ " 📈")
                              else
                                text label
                            ]
                    )
    in
        table [ class "table table-striped table-responsive" ]
            [ thead []
                (th [] [ text "Name" ] :: sortableHeaders)
            , (currencyList
                |> List.map
                    (\currency ->
                        ( toString currency.id
                        , lazy viewTickerRow currency
                        )
                    )
                |> Html.Keyed.node "tbody" []
              )
            ]


viewTickerRow : Currency -> Html Msg
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

        valueStyles : Float -> Html.Attribute Msg
        valueStyles val =
            style [ ( "color", colorIndicator val ) ]

        iconSrc =
            "https://s2.coinmarketcap.com/static/img/coins/32x32/" ++ (toString currency.id) ++ ".png"

        { price, percentChange1h, percentChange24h, percentChange7d, volume24h } =
            currency.quotes
    in
        tr []
            [ td [ class "currency-name" ]
                [ a
                    [ href ("https://coinmarketcap.com/currencies/" ++ currency.websiteSlug)
                    , target "_blank"
                    ]
                    [ img [ src iconSrc ] []
                    , text currency.name
                    ]
                ]
            , td [] [ text <| "$" ++ (dynamicRound price) ]
            , td [ (valueStyles percentChange1h) ] [ text <| (toString percentChange1h) ++ "%" ]
            , td [ (valueStyles percentChange24h) ] [ text <| (toString percentChange24h) ++ "%" ]
            , td [ (valueStyles percentChange7d) ] [ text <| (toString percentChange7d) ++ "%" ]
            , td [] [ text <| "$" ++ (format { usLocale | decimals = 0 } volume24h) ]
            ]
