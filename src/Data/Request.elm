module Data.Request exposing (RequestState(..))


type RequestState a
    = Idle
    | Loading
    | FinishedLoading a
    | Errored String
