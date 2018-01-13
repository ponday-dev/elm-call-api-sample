module Qiita.My.Items exposing (Response, Item, getMyItems)

import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)

import Qiita.Endpoints exposing (myItems)

type alias Request =
    { token: String
    , page: Int
    , perPage: Int
    }

type alias Response =
    List Item

type alias Item =
    { title: String
    , url: String
    }

itemDecoder: Decoder Item
itemDecoder =
    Decode.succeed Item
        |> requiredAt [ "title" ] Decode.string
        |> requiredAt [ "url" ] Decode.string


responseDecoder: Decoder Response
responseDecoder =
    Decode.list itemDecoder


getMyItems: (Result Error Response -> a) -> Request -> Cmd a
getMyItems msg request =
    let
        req =
            Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" ("Bearer " ++ request.token)
                    ]
                , url = myItems request.page request.perPage
                , body = Http.emptyBody
                , expect = Http.expectJson responseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send msg req

