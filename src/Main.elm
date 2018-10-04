port module Main exposing (main)

-- PORTS

import Base64
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (toTask)
import Json.Decode as Decode exposing (Decoder, andThen, array, dict, field, list, map, map2, map3, map4, map5, map6, string, succeed)
import Json.Encode as E
import Process exposing (spawn)
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
    , detailsRequests : Int
    , loadingReport : Bool
    }


init : Maybe String -> ( Model, Cmd Msg )
init authToken =
    case authToken of
        Just token ->
            ( Model "" "" token True Dict.empty Nothing 1 0 False
            , Cmd.none
            )

        Nothing ->
            ( Model "" "" "" False Dict.empty Nothing 1 0 False
            , Cmd.none
            )


type Msg
    = LoginChange String
    | PasswordChange String
    | GetToken
    | SaveBarCode String
    | TokenCheck (Result Http.Error String)
    | LoadReport
    | ProductDetailsLoaded (Result Http.Error ProductDetails)
    | ReportLoaded (Result Http.Error Report)
    | UpdateProductBarcode String String
    | BarcodeUpdated (Result Http.Error ProductDetails)



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
            ( { model | loadingReport = True }
            , Http.send ReportLoaded (loadReport model.token model.page)
            )

        ProductDetailsLoaded (Ok details) ->
            let
                enoughProducts =
                    Dict.size (filterOnlyMissingBarcodes model.products) > 10

                newPage =
                    if enoughProducts then
                        model.page

                    else
                        model.page + 1
            in
            if model.detailsRequests == 1 then
                ( { model
                    | detailsRequests = 0
                    , products = addProductDetails details model.products
                    , page = newPage
                  }
                , if enoughProducts then
                    Cmd.none

                  else
                    Http.send ReportLoaded (loadReport model.token newPage)
                )

            else
                ( { model
                    | detailsRequests = model.detailsRequests - 1
                    , products = addProductDetails details model.products
                  }
                , Cmd.none
                )

        ProductDetailsLoaded (Err err) ->
            ( model, Cmd.none )

        ReportLoaded (Ok report) ->
            ( { model
                | products = Dict.union report model.products
                , detailsRequests = Dict.size report
                , loadingReport = False
              }
            , loadDetailsForReport model report
                |> Cmd.batch
            )

        ReportLoaded (Err error) ->
            ( { model | err = Just error }, Cmd.none )

        SaveBarCode productCode ->
            let
                productDetails =
                    Dict.get productCode model.products
                        |> Maybe.andThen .details
            in
            ( model, updateBarcode model.token productDetails )

        BarcodeUpdated (Ok details) ->
            ( { model | products = Dict.update details.code updateSaved model.products }, Cmd.none )

        BarcodeUpdated (Err err) ->
            ( { model | err = Just err }, Cmd.none )

        UpdateProductBarcode productCode newBarcode ->
            ( { model | products = Dict.update productCode (updateBarcodes newBarcode) model.products }, Cmd.none )


updateBarcode : String -> Maybe ProductDetails -> Cmd Msg
updateBarcode token productDetails =
    case productDetails of
        Just val ->
            Http.send BarcodeUpdated (sendBarcodeUpdate token val)

        Nothing ->
            Cmd.none


addProductDetailsResultsToReport : Report -> List ProductDetails -> Report
addProductDetailsResultsToReport report productDetails =
    List.foldl addProductDetails report productDetails



-- addProductDetailsToTasks: Report -> List Task Http.Error ProductDetails -> List Task Http.Error


addProductDetailsToReport : Report -> Task Http.Error ProductDetails -> Task Http.Error Report
addProductDetailsToReport report detailsTask =
    Task.map (\details -> addProductDetails details report) detailsTask


type alias Report =
    Dict String ReportRow


loadDetailsForReport : Model -> Report -> List (Cmd Msg)
loadDetailsForReport model report =
    prepareRequests model.token report
        |> List.map (Task.attempt ProductDetailsLoaded)


loadReport : String -> Int -> Http.Request Report
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


filterChangedOrMissingBarcodes report =
    Dict.filter (\code row -> rowHasMissingBarcodes code row || rowIsChanged code row) report


rowIsChanged : String -> ReportRow -> Bool
rowIsChanged code row =
    case row.details of
        Just productDetails ->
            row.changed

        Nothing ->
            False


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


updateSaved : Maybe ReportRow -> Maybe ReportRow
updateSaved reportRow =
    Maybe.map (\row -> { row | saved = True }) reportRow


updateBarcodes : String -> Maybe ReportRow -> Maybe ReportRow
updateBarcodes barcode reportRow =
    Maybe.map
        (\row ->
            { row
                | saved = False
                , changed = True
                , details = updateBarcodeWithinProductDetails barcode row.details
            }
        )
        reportRow


updateBarcodeWithinProductDetails : String -> Maybe ProductDetails -> Maybe ProductDetails
updateBarcodeWithinProductDetails barcode productDetails =
    Maybe.map (\details -> { details | barcodes = [ barcode ] }) productDetails


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
    map4 ProductDetails
        (field "code" string)
        (field "barcodes" (list string))
        (field "name" string)
        (field "id" string)


type alias ProductDetails =
    { code : String
    , barcodes : List String
    , name : String
    , id : String
    }


reportSalesByVariant : Int -> String
reportSalesByVariant page =
    getApiUrl
        [ "report"
        , "sales"
        , "byvariant"
        ]
        (Just [ Url.Builder.int "offset" (page * 25), Url.Builder.int "limit" 50 ])


sendBarcodeUpdate : String -> ProductDetails -> Http.Request ProductDetails
sendBarcodeUpdate token details =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = getApiUrl [ "entity", "product", details.id ] Nothing
        , body = barcodeEncoderForReal details
        , expect = Http.expectJson barCodeEncoder
        , timeout = Nothing
        , withCredentials = False
        }


barcodeEncoderForReal : ProductDetails -> Http.Body
barcodeEncoderForReal productDetails =
    Http.jsonBody
        (E.object
            [ ( "barcodes", E.list E.string productDetails.barcodes )
            ]
        )



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
    , id : Maybe String
    , saved : Bool
    , changed : Bool
    }


reportRowDecoder : Decode.Decoder ReportRow
reportRowDecoder =
    map6 ReportRow
        (field "code" string)
        (field "meta" (field "href" string))
        (succeed Nothing)
        (succeed Nothing)
        (succeed True)
        (succeed False)


getApiUrl : List String -> Maybe (List QueryParameter) -> String
getApiUrl parameters queryParemeters =
    getProxyUrl (List.append [ "https://online.moysklad.ru", "api", "remap", "1.1" ] parameters) (Maybe.withDefault [] queryParemeters)


getProxyUrl : List String -> List QueryParameter -> String
getProxyUrl =
    crossOrigin "https://secure-fjord-80419.herokuapp.com"



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
            let
                barcode =
                    Maybe.withDefault "" (List.head productDetails.barcodes)

                color =
                    if product.changed && product.saved then
                        "lightgreen"

                    else if product.changed then
                        "lightyellow"

                    else
                        "white"
            in
            li [ class "item mdl-card mdl-shadow--2dp", style "background" color ]
                [ span [ class "mdl-card__title-text" ] [ text productDetails.name ]
                , Html.form [ onSubmit (SaveBarCode productDetails.code) ]
                    [ div
                        [ class "mdl-textfield" ]
                        [ input
                            [ onInput (UpdateProductBarcode productDetails.code)
                            , type_ "text"
                            , value barcode
                            , class "mdl-textfield__input"
                            , id ("barcode" ++ productDetails.code)
                            ]
                            []
                        , label [ class "", for ("barcode" ++ productDetails.code) ] [ text "Штрихкод" ]
                        ]
                    , div [ class "mdl-card__actions mdl-card--border" ]
                        [ input
                            [ class "mdl-button mdl-button--colored mdl-js-button mdl-js-ripple-effect"
                            , type_ "submit"
                            , value "Сохранить"
                            , tabindex -1
                            ]
                            []
                        ]
                    ]
                ]

        Nothing ->
            li [] []


view : Model -> Html Msg
view model =
    if not model.loggedIn then
        div [ class "loginWrapper" ]
            [ Html.form [ class "loginForm", onSubmit GetToken ]
                [ input
                    [ type_ "text"
                    , placeholder "Логин"
                    , value model.login
                    , onInput LoginChange
                    , class "mdl-textfield__input"
                    ]
                    []
                , input
                    [ type_ "password"
                    , placeholder "Пароль"
                    , value model.password
                    , onInput PasswordChange
                    , class "mdl-textfield__input"
                    ]
                    []
                , input
                    [ type_ "submit"
                    , value "Войти"
                    , class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent mdl-js-ripple-effect"
                    ]
                    []
                ]
            ]

    else
        div []
            [ header
                []
                [ div []
                    [ h3 []
                        [ text "Вы вошли в систему" ]
                    ]
                , button
                    [ class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent mdl-js-ripple-effect"
                    , onClick LoadReport
                    ]
                    [ text "Загрузить данные" ]
                ]
            , Html.main_ [] [ renderListOrLoading model.detailsRequests model.loadingReport (filterChangedOrMissingBarcodes model.products) ]
            ]


renderListOrLoading : Int -> Bool -> Report -> Html Msg
renderListOrLoading detailsRequests loadingReport report =
    if (detailsRequests > 1) || loadingReport then
        div [ class "loading" ]
            [ span [] [ text "Загружается..." ] ]

    else
        ul [ class "items" ] (List.map renderProduct (Dict.values report))


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
