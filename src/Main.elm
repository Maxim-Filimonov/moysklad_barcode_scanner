port module Main exposing (main)

-- PORTS

import Base64
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (toTask)
import Json.Decode as Decode exposing (Decoder, andThen, array, dict, field, list, map, map2, map3, string, succeed)
import Json.Encode as E
import Task exposing (Task, perform, sequence)
import Url.Builder exposing (QueryParameter, crossOrigin)


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
    , products : Report
    , err : Maybe Http.Error
    , page : Int
    }


init : Maybe String -> ( Model, Cmd Msg )
init authToken =
    case authToken of
        Just token ->
            ( Model "" "" token True Dict.empty Nothing 1
            , Cmd.none
            )

        Nothing ->
            ( Model "" "" "" False Dict.empty Nothing 1
            , Cmd.none
            )


type Msg
    = LoginChange String
    | PasswordChange String
    | GetToken
    | TokenCheck (Result Http.Error String)
    | LoadReport
    | ProductData (Result Http.Error Report)



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
            , Http.toTask (loadReport model.token model.page)
                |> Task.andThen
                    (\report ->
                        Task.map (addProductDetailsResultsToReport report) (Task.sequence (prepareRequests model.token report))
                    )
                |> Task.attempt ProductData
            )

        ProductData (Ok report) ->
            loadMore model report

        ProductData (Err err) ->
            ( model, Cmd.none )


addProductDetailsResultsToReport : Report -> List ProductDetails -> Report
addProductDetailsResultsToReport report productDetails =
    List.foldl addProductDetails report productDetails


addProductDetailsToReport : Report -> Task Http.Error ProductDetails -> Task Http.Error Report
addProductDetailsToReport report detailsTask =
    Task.map (\details -> addProductDetails details report) detailsTask


type alias Report =
    Dict String ReportRow


loadMore : Model -> Report -> ( Model, Cmd Msg )
loadMore model report =
    let
        numberOfValidProducts =
            filterOnlyMissingBarcodes model.products
                |> Dict.values
                |> List.length
    in
    if numberOfValidProducts < 5 then
        ( { model
            | page = model.page + 1
            , products = Dict.union model.products report
          }
        , Http.send ProductData (loadReport model.token model.page)
        )

    else
        ( model, Cmd.none )


loadReport token page =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = reportSalesByVariant page
        , body = Http.emptyBody
        , expect = Http.expectJson productCodeEncoder
        , timeout = Nothing
        , withCredentials = False
        }


filterOnlyMissingBarcodes : Report -> Report
filterOnlyMissingBarcodes report =
    Dict.filter rowHasMissingBarcodes report


rowHasMissingBarcodes : String -> ReportRow -> Bool
rowHasMissingBarcodes code row =
    case row.details of
        Just productDetails ->
            missingBarcodes productDetails.barcodes

        Nothing ->
            False


addProductDetails : ProductDetails -> Report -> Report
addProductDetails productDetails products =
    Dict.update productDetails.code (updateProductDetails productDetails) products


updateProductDetails : ProductDetails -> Maybe ReportRow -> Maybe ReportRow
updateProductDetails productDetails reportRow =
    Maybe.map (\row -> { row | details = Just productDetails }) reportRow


prepareRequests : String -> Report -> List (Task Http.Error ProductDetails)
prepareRequests token links =
    Dict.values (Dict.map (\key value -> Http.toTask (createHttpRequest token value)) links)


createHttpRequest : String -> ReportRow -> Http.Request ProductDetails
createHttpRequest token reportRow =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = getProxyUrl [ reportRow.href ] []
        , body = Http.emptyBody
        , expect = Http.expectJson barCodeEncoder
        , timeout = Nothing
        , withCredentials = False
        }


getProducts : String
getProducts =
    getApiUrl [ "entity", "product" ] Nothing


barCodeEncoder : Decode.Decoder ProductDetails
barCodeEncoder =
    map3 ProductDetails
        (field "code" string)
        (field "barcodes" (list string))
        (field "name" string)


type alias ProductDetails =
    { code : String
    , barcodes : List String
    , name : String
    }


reportSalesByVariant : Int -> String
reportSalesByVariant page =
    getApiUrl [ "sales", "byvariant" ] (Just [ Url.Builder.string "offset" (String.fromInt (page * 25)) ])



-- "https://online.moysklad.ru/api/remap/1.1/report/sales/byvariant"


productCodeEncoder : Decode.Decoder Report
productCodeEncoder =
    field "rows" (list assortmentDecoder)
        |> map (List.map (\row -> ( row.code, row )))
        |> map Dict.fromList



-- |> map Dict.fromList
-- map Dict.fromList (dict (field "rows" (list assortmentDecoder)))


assortmentDecoder : Decode.Decoder ReportRow
assortmentDecoder =
    field "assortment" reportRowDecoder


type alias ReportRow =
    { code : String
    , href : String
    , details : Maybe ProductDetails
    }


reportRowDecoder : Decode.Decoder ReportRow
reportRowDecoder =
    map3 ReportRow
        (field "code" string)
        (field "meta" (field "href" string))
        (succeed Nothing)


getApiUrl : List String -> Maybe (List QueryParameter) -> String
getApiUrl parameters queryParemeters =
    getProxyUrl (List.append [ "https://online.moysklad.ru", "api", "remap", "1.1" ] parameters) (Maybe.withDefault [] queryParemeters)


getProxyUrl : List String -> List QueryParameter -> String
getProxyUrl =
    crossOrigin "http://localhost:8080"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


missingBarcodes : List String -> Bool
missingBarcodes barcodes =
    List.all isGeneratedBarcode barcodes


isGeneratedBarcode : String -> Bool
isGeneratedBarcode barcode =
    String.startsWith "2000" barcode


renderProduct : ReportRow -> Html Msg
renderProduct product =
    case product.details of
        Just productDetails ->
            if missingBarcodes productDetails.barcodes then
                li [ style "color" "green" ] [ text productDetails.name ]

            else
                li [ style "color" "red" ] [ text productDetails.name ]

        Nothing ->
            li [] []


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
            , ul [] (List.map renderProduct (Dict.values model.products))
            ]


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
