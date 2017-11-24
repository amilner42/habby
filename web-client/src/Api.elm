module Api exposing (..)

import DefaultServices.Http exposing (post)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.ApiError exposing (ApiError)


{-| Send the `query` to the graphql endpoint.
-}
graphQLRequest : String -> String -> Decode.Decoder a -> (ApiError -> b) -> (a -> b) -> Cmd b
graphQLRequest url query decoder handleError handleSuccess =
    post url decoder (Encode.string query) handleError handleSuccess
