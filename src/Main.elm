module Main exposing (main)

import Base64
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { login : String
    , password : String
    , token : String
    , loggedIn : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" "" False
    , Cmd.none
    )


type Msg
    = LoginChange String
    | PasswordChange String
    | GetToken
    | TokenCheck (Result Http.Error String)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginChange newLogin ->
            ( { model | login = newLogin }
            , Cmd.none
            )

        PasswordChange newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        GetToken ->
            let
                token =
                    "Basic " ++ buildAuthorizationToken model.login model.password
            in
            ( { model | token = token }
            , Http.send TokenCheck <|
                Http.request
                    { method = "GET"
                    , headers =
                        [ Http.header "Authorization" token
                        ]
                    , url = getProducts
                    , body = Http.emptyBody
                    , expect = Http.expectString
                    , timeout = Nothing
                    , withCredentials = False
                    }
            )

        TokenCheck (Ok product) ->
            ( { model | loggedIn = True }, Cmd.none )

        TokenCheck (Err _) ->
            ( model, Cmd.none )


getProducts =
    getProxiedUrl "https://online.moysklad.ru/api/remap/1.1/entity/product"


getProxiedUrl : String -> String
getProxiedUrl url =
    "http://localhost:8080/" ++ url



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    if not model.loggedIn then
        Html.form [ onSubmit GetToken ]
            [ input
                [ type_ "text"
                , placeholder "Login"
                , value model.login
                , onInput LoginChange
                ]
                []
            , input
                [ type_ "password"
                , placeholder "Password"
                , value model.password
                , onInput PasswordChange
                ]
                []
            , input
                [ type_ "submit"
                , value "Login"
                ]
                []
            ]

    else
        h1 [] [ text "You are logged in" ]


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
