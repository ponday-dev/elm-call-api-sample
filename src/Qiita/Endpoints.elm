module Qiita.Endpoints exposing (..)

baseUrl: String
baseUrl =
    "https://qiita.com"


authorize: String -> String -> String
authorize clientId scope =
    baseUrl
        ++ "/api/v2/oauth/authorize"
        ++ "?client_id="
        ++ clientId
        ++ "&scope="
        ++ scope


accessTokens: String
accessTokens =
    baseUrl ++ "/api/v2/access_tokens"


myItems: Int -> Int -> String
myItems page perPage =
    baseUrl
        ++ "/api/v2/authenticated_user/items"
        ++ "?page="
        ++ (toString page)
        ++ "&per_page="
        ++ (toString perPage)

