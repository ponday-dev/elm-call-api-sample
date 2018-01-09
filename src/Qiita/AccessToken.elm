module Qiita.AccessToken exposing
    ( Request
    , Response
    , getAccessToken
    )

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Json.Encode as Encode

import Qiita.Endpoints exposing (accessTokens)


-- request of POST /api/v2/access_tokens
type alias Request =
    { clientId: String
    , clientSecret: String
    , code: String
    }


-- response of POST /api/v2/access_tokens
type alias Response =
    { clientId: String
    , scopes: List String
    , token: String
    }


-- encoder
requestEncoder: Request -> Encode.Value
requestEncoder request =
    Encode.object
        [ ("client_id", Encode.string request.clientId)
        , ("client_secret", Encode.string request.clientSecret)
        , ("code", Encode.string request.code)
        ]


-- decoder
responseDecoder: Decoder Response
responseDecoder =
    Decode.succeed Response
        |> requiredAt [ "client_id" ] Decode.string
        |> requiredAt [ "scopes" ] (Decode.list Decode.string)
        |> requiredAt [ "token" ] Decode.string


-- post request
getAccessToken: (Result Http.Error Response -> a) -> Request -> Cmd a
getAccessToken msg requestBody =
    requestBody
        |> requestEncoder
        |> Encode.encode 4
        |> Http.stringBody "application/json"
        |> Http.post accessTokens
        |> (|>) responseDecoder
        |> Http.send msg

