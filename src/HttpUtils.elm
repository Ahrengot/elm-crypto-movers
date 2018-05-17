module HttpUtils exposing (..)

import Http


parseError : Http.Error -> String
parseError err =
    case err of
        Http.Timeout ->
            "Request timeout. Please try again."

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error. Check your internet connection and try agian."

        Http.BadStatus res ->
            "Bad status: " ++ res.body ++ " (code " ++ (toString res.status) ++ ")"

        Http.BadPayload err res ->
            "Bad payload: " ++ err
