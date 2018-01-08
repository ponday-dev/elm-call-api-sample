module Main exposing (..)

import Html exposing (Html, text, div, button, a)
import Html.Attributes exposing (href)
-- import Html.Events exposing (onClick)

import Task
import Navigation exposing (Location, newUrl)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (requiredAt)
import Json.Encode as Encode

import Routing exposing (Route(..), routeToPath)
import Endpoints exposing (Endpoints)
import Secrets

---- MODEL ----


type alias Model =
    { route: Route
    , endpoints: Endpoints
    , accessToken: String
    }


initialModel: Route -> Model
initialModel route =
    { route = route
    , endpoints = Endpoints.init "https://qiita.com/"
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
    | RedirectToQiita
    | ClickLink String
    | GetAccessToken String
    | GotAccessToken (Result Http.Error AccessTokenResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []
        LocationChange location ->
            (parseLocation (Just model) location) ! []
        RedirectToQiita ->
            model ! [send <| ClickLink (model.endpoints.login Secrets.clientId)]
        ClickLink url ->
            model ! [newUrl url]
        GetAccessToken code ->
            model ! [getAccessToken model Secrets.clientId Secrets.clientSecret code]
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
        [ a [ href (model.endpoints.login Secrets.clientId) ] [ text "Login to Qiita" ]
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



getAccessToken: Model -> String -> String -> String -> Cmd Msg
getAccessToken model clientId clientSecret code =
    let
        url =
            model.endpoints.accessToken
        encode =
            accessTokenEncoder clientId clientSecret code
        body =
            Http.stringBody "application/json" (Encode.encode 4 encode)
        request =
            Http.post url body accessTokenDecoder
    in
        Http.send GotAccessToken request


type alias AccessTokenResponse =
    { clientId: String
    , scopes: List String
    , token: String
    }


accessTokenDecoder: Decoder AccessTokenResponse
accessTokenDecoder =
    Decode.succeed AccessTokenResponse
        |> requiredAt [ "client_id" ] Decode.string
        |> requiredAt [ "scopes" ] (Decode.list Decode.string)
        |> requiredAt [ "token" ] Decode.string


accessTokenEncoder: String -> String -> String -> Encode.Value
accessTokenEncoder clientId clientSecret code =
    Encode.object
        [ ("client_id", Encode.string clientId)
        , ("client_secret", Encode.string clientSecret)
        , ("code", Encode.string code)
        ]


send: Msg -> Cmd Msg
send msg =
    Task.succeed msg
        |> Task.perform identity
