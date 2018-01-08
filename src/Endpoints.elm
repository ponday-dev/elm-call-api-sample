module Endpoints exposing (..)


type alias Endpoints =
    { login: String -> String
    , accessToken: String
    }


init: String -> Endpoints
init baseUrl =
    { login = login baseUrl
    , accessToken = accessToken baseUrl
    }

login: String -> String -> String
login baseUrl clientId =
    let
        url =
            baseUrl ++ "/api/v2/oauth/authorize"
        params =
            "?client_id=" ++ clientId
                 ++ "&scope=read_qiita"
    in
        url ++ params


accessToken: String -> String
accessToken baseUrl =
    baseUrl ++ "/api/v2/access_tokens"
