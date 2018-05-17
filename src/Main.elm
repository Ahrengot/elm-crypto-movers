module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Data.Currency exposing (Currency)
import Data.Request exposing (RequestState(..))
import Request.Ticker
import Http
import HttpUtils


---- MODEL ----


type alias Model =
    { listRequest : RequestState (List Currency)
    }


init : ( Model, Cmd Msg )
init =
    ( { listRequest = Loading }
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
            let
                currencies =
                    List.map Tuple.second currencyTuples
                        |> List.sortBy .rank
            in
                ( { model
                    | listRequest = FinishedLoading currencies
                  }
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container-fluid" ]
            [ case model.listRequest of
                Idle ->
                    text "Welcome!"

                Loading ->
                    text "Loading ticker data..."

                FinishedLoading currencies ->
                    List.map viewCurrency currencies
                        |> div [ class "currency-list" ]

                Errored errMsg ->
                    div [ class "alert alert-danger" ]
                        [ h4 [ class "alert-heading" ] [ text "Error loading tickers" ]
                        , div [] [ text errMsg ]
                        ]
            ]
        ]


viewCurrency : Currency -> Html Msg
viewCurrency currency =
    div []
        [ a
            [ href ("https://coinmarketcap.com/currencies/" ++ currency.websiteSlug)
            , target "_blank"
            ]
            [ text currency.name
            ]
        ]
