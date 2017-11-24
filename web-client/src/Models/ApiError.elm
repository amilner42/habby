module Models.ApiError exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode


{-| An error from the api still in json form.
-}
type alias BackendError =
    { message : String
    , errorCode : Int
    }


{-| An error from the api represented as a union of possible errors.
-}
type ApiError
    = UnexpectedPayload
    | RawTimeout
    | RawNetworkError
    | InternalError


{-| Turns an errorCode integer from the backend to it's respective ApiError.
-}
fromErrorCode : Int -> ApiError
fromErrorCode errorCode =
    case errorCode of
        _ ->
            InternalError


{-| `ApiError` decoder.
-}
decoder : Decode.Decoder ApiError
decoder =
    let
        backendDecoder =
            decode BackendError
                |> required "message" Decode.string
                |> required "errorCode" Decode.int

        backendErrorToApiError { errorCode } =
            fromErrorCode errorCode
    in
    Decode.map backendErrorToApiError backendDecoder
