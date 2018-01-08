module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, parsePath ,oneOf, map, s, top, string, (<?>), stringParam)

type Route
    = TopRoute
    | LoginRoute (Maybe String)
    | MainRoute
    | NotFoundRoute


matcher: Parser (Route -> a) a
matcher =
    oneOf
        [ map TopRoute top
        , map LoginRoute (s (routeToPath <| LoginRoute <| Just "") <?> stringParam "code")
        , map MainRoute (s "main")
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
        MainRoute ->
            "main"
        NotFoundRoute ->
            "404"
