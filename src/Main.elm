module Main exposing (..)

import Html exposing (Html, text, div, button, a)
import Html.Attributes exposing (href)
-- import Html.Events exposing (onClick)

import Task
import Navigation exposing (Location, newUrl)
import Http

import Qiita.Endpoints
import Qiita.AccessToken
import Routing exposing (Route(..), routeToPath)
import Secrets

---- MODEL ----


type alias Model =
    { route: Route
    , accessToken: String
    }


initialModel: Route -> Model
initialModel route =
    { route = route
    , accessToken = ""
    }

init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            parseLocation Nothing location
        msg =
            case model.route of
                LoginRoute param ->
                    case param of
                        Just code ->
                            GetAccessToken code
                        Nothing ->
                            ClickLink (Routing.routeToPath NotFoundRoute)
                _ ->
                    Noop
        cmd =
            send msg
    in
        model ! [cmd]


parseLocation: (Maybe Model) -> Location -> Model
parseLocation model location =
    let
        route =
            Routing.parseLocation location
        currentModel =
            case model of
                Just value ->
                    value
                Nothing ->
                    initialModel route
    in
        { currentModel | route = route }

---- UPDATE ----


type Msg
    = Noop
    | LocationChange Location
    | ClickLink String
    | GetAccessToken String
    | GotAccessToken (Result Http.Error Qiita.AccessToken.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []
        LocationChange location ->
            (parseLocation (Just model) location) ! []
        ClickLink url ->
            model ! [newUrl url]
        GetAccessToken code ->
            let
                request =
                    { clientId = Secrets.clientId
                    , clientSecret = Secrets.clientSecret
                    , code = code
                    }
                cmd =
                    Qiita.AccessToken.getAccessToken GotAccessToken request
            in
                model ! [cmd]
        GotAccessToken (Ok response) ->
            { model | accessToken = response.token } ! [Navigation.newUrl "/main"]
        GotAccessToken (Err e) ->
            model ! [Navigation.newUrl "/"]


---- VIEW ----


view : Model -> Html Msg
view model =
    case model.route of
        TopRoute ->
            top model
        LoginRoute _ ->
            div [] []
        MainRoute ->
            div [] [ text "logined" ]
        NotFoundRoute ->
            div [] [ text "Not Found" ]


top: Model -> Html Msg
top model =
    div []
        [ a [ href (Qiita.Endpoints.authorize Secrets.clientId "read_qiita") ] [ text "Login to Qiita" ]
        ]

---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


send: Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity
