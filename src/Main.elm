module Main exposing (..)

import Html exposing (Html, text, div, button, a, ul, li, header, h3)
import Html.Attributes exposing (href, target, class)
-- import Html.Events exposing (onClick)

import Task
import Navigation exposing (Location, newUrl)
import Http

import Qiita.Endpoints
import Qiita.AccessToken
import Qiita.My.Items
import Routing exposing (Route(..), routeToPath)
import Secrets

---- MODEL ----


type alias Model =
    { route: Route
    , accessToken: String
    , articles: List Qiita.My.Items.Item
    }


initialModel: Route -> Model
initialModel route =
    { route = route
    , accessToken = ""
    , articles = []
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
                    if model.accessToken == "" then
                        ClickLink (routeToPath TopRoute)
                    else
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
    | GetMyItems
    | GotMyItems (Result Http.Error Qiita.My.Items.Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []
        LocationChange location ->
            let
                checkToken =
                    if model.accessToken /= "" then
                        send <| ClickLink (routeToPath ArticlesRoute)
                    else
                        Cmd.none
                newModel =
                    parseLocation (Just model) location
                cmd =
                    case newModel.route of
                        LoginRoute _ ->
                            checkToken
                        TopRoute ->
                            checkToken
                        ArticlesRoute ->
                            if model.accessToken == "" then
                                send <| ClickLink (routeToPath <| LoginRoute Nothing)
                            else
                                send GetMyItems
                        _ ->
                            if model.accessToken == "" then
                                send <| ClickLink (routeToPath <| LoginRoute Nothing)
                            else
                                Cmd.none
            in
                newModel ! [cmd]
        ClickLink url ->
            model ! [newUrl ("/" ++ url)]
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
            { model | accessToken = response.token } ! [Navigation.newUrl "/articles"]
        GotAccessToken (Err e) ->
            model ! [Navigation.newUrl "/"]
        GetMyItems ->
            let
                request =
                    { token = model.accessToken
                    , page = 1
                    , perPage = 20
                    }
                cmd =
                    Qiita.My.Items.getMyItems GotMyItems request
            in
                model ! [cmd]
        GotMyItems (Ok items) ->
            { model | articles = items } ! []
        GotMyItems (Err e) ->
            { model | articles = [] } ! []


---- VIEW ----


view : Model -> Html Msg
view model =
    case model.route of
        TopRoute ->
            top model
        LoginRoute _ ->
            div [] []
        ArticlesRoute ->
            articles model
        NotFoundRoute ->
            div [] [ text "Not Found" ]


top: Model -> Html Msg
top model =
    div [ class "top" ]
        [ a [ href (Qiita.Endpoints.authorize Secrets.clientId "read_qiita")
            , class "login"
            ]
            [ text "Login to Qiita" ]
        ]


articles: Model -> Html Msg
articles model =
    let
        items =
            model.articles
                |> List.map itemLink
                |> ul [ class "articles__list" ]
    in
        div [ class "articles" ]
            [ header [ class "header" ]
                [ h3 [] [ text "あなたの記事" ]
                ]
            , items
            ]


itemLink: Qiita.My.Items.Item -> Html Msg
itemLink article =
    li [ class "articles__item" ]
        [ a [ href article.url
            , target "blank"
            , class "articles__link"
            ]
            [ text article.title ]
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
