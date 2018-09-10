port module Main exposing (main)

-- PORTS

import Base64
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (toTask)
import Json.Decode as Decode exposing (array, field, list, map2, string)
import Json.Encode as E
import List exposing (map)
import Task exposing (Task, perform, sequence)


port setToken : String -> Cmd msg



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
    , products : List String
    , productBarcodes : List (List String)
    }


init : Maybe String -> ( Model, Cmd Msg )
init authToken =
    case authToken of
        Just token ->
            ( Model "" "" token True [] []
            , Cmd.none
            )

        Nothing ->
            ( Model "" "" "" False [] []
            , Cmd.none
            )


type Msg
    = LoginChange String
    | PasswordChange String
    | GetToken
    | TokenCheck (Result Http.Error String)
    | LoadReport
    | SalesReport (Result Http.Error (List String))
    | ProductData (Result Http.Error (List (List String)))



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
            ( { model | loggedIn = True }, setToken model.token )

        TokenCheck (Err _) ->
            ( model, Cmd.none )

        LoadReport ->
            ( model
            , Http.send SalesReport <|
                Http.request
                    { method = "GET"
                    , headers =
                        [ Http.header "Authorization" model.token
                        ]
                    , url = reportSalesByVariant
                    , body = Http.emptyBody
                    , expect = Http.expectJson productCodeEncoder
                    , timeout = Nothing
                    , withCredentials = False
                    }
            )

        SalesReport (Ok linksToProducts) ->
            ( { model | products = linksToProducts }
            , Task.attempt ProductData (sequence (prepareRequests model.token linksToProducts))
            )

        SalesReport (Err err) ->
            ( model, Cmd.none )

        ProductData (Ok productBarcodes) ->
            ( { model | productBarcodes = productBarcodes }
            , Cmd.none
            )

        ProductData (Err err) ->
            ( model, Cmd.none )


prepareRequests : String -> List String -> List (Task Http.Error (List String))
prepareRequests token links =
    map toTask (requestForEachLink token links)


requestForEachLink : String -> List String -> List (Http.Request (List String))
requestForEachLink token links =
    map (hasBarcode token) links


hasBarcode token ref =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = getProxiedUrl ref
        , body = Http.emptyBody
        , expect = Http.expectJson barCodeEncoder
        , timeout = Nothing
        , withCredentials = False
        }


getProducts =
    getProxiedUrl "https://online.moysklad.ru/api/remap/1.1/entity/product"


barCodeEncoder =
    field "barcodes" (list string)


reportSalesByVariant =
    getProxiedUrl "https://online.moysklad.ru/api/remap/1.1/report/sales/byvariant"


productCodeEncoder =
    field "rows" (list referenceDecoder)


referenceDecoder =
    field "assortment" (field "meta" (field "href" string))


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
        div []
            [ h1 []
                [ text "You are logged in" ]
            , button
                [ onClick LoadReport ]
                [ text "Load Report" ]
            ]


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
