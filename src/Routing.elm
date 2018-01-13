module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parsePath ,oneOf, map, s, top, string, (<?>), stringParam)

type Route
    = TopRoute
    | LoginRoute (Maybe String)
    | ArticlesRoute
    | NotFoundRoute


matcher: Parser (Route -> a) a
matcher =
    oneOf
        [ map TopRoute top
        , map LoginRoute (s (routeToPath <| LoginRoute <| Just "") <?> stringParam "code")
        , map ArticlesRoute (s <| routeToPath ArticlesRoute)
        ]


parseLocation: Location -> Route
parseLocation location =
    case (parsePath matcher location) of
        Just route ->
            route
        Nothing ->
            NotFoundRoute


routeToPath: Route -> String
routeToPath route =
    case route of
        TopRoute ->
            ""
        LoginRoute _ ->
            "login"
        ArticlesRoute ->
            "articles"
        NotFoundRoute ->
            "404"
