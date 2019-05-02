port module Main exposing (main)

-- PORTS

import Base64
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h2, h3, header, input, label, li, p, span, text, ul)
import Html.Attributes exposing (class, disabled, for, id, placeholder, style, tabindex, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Maybe.Extra exposing (isJust, values)
import Model exposing (ProductDetails, RemainsInfo, Report, ReportRow)
import Parsers exposing (barCodeDecoder, barCodeDecoderForReal, productCodeDecoder, remainsInfoListDecoder)
import Task exposing (Task)
import Url.Builder exposing (QueryParameter, crossOrigin)


port setToken : String -> Cmd msg



-- MAIN


main : Program (Maybe String) Model Msg
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
    , errorMessage : Maybe String
    , page : Int
    , loadingReport : Bool
    , loginInProgress : Bool
    }


init : Maybe String -> ( Model, Cmd Msg )
init authToken =
    case authToken of
        Just token ->
            ( { login = ""
              , password = ""
              , token = token
              , loggedIn = True
              , products = Dict.empty
              , err = Nothing
              , errorMessage = Nothing
              , page = 1
              , loadingReport = False
              , loginInProgress = False
              }
              -- Model "" "" token True Dict.empty Nothing 1 False
            , Cmd.none
            )

        Nothing ->
            ( { login = ""
              , password = ""
              , token = ""
              , loggedIn = False
              , products = Dict.empty
              , err = Nothing
              , errorMessage = Nothing
              , page = 1
              , loadingReport = False
              , loginInProgress = False
              }
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
    | LoadedRemains (Result Http.Error (Dict String RemainsInfo))



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
            ( { model | token = token, loginInProgress = True }
            , Http.request
                { method = "GET"
                , headers =
                    [ Http.header "Authorization" token
                    ]
                , url = getProducts
                , body = Http.emptyBody
                , expect = Http.expectString TokenCheck
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        TokenCheck (Ok _) ->
            ( { model | loggedIn = True, loginInProgress = False, errorMessage = Nothing }, setToken model.token )

        TokenCheck (Err err) ->
            let
                errorMessage =
                    case err of
                        Http.BadStatus status ->
                            if status == 401 then
                                "Неправильный логин или пароль"

                            else
                                "Неизвестная ошибка, статус:" ++ String.fromInt status

                        _ ->
                            "Неизвестная ошибка"
            in
            ( { model | errorMessage = Just errorMessage, loginInProgress = False }, Cmd.none )

        LoadReport ->
            ( { model | loadingReport = True }
            , loadReport model.token model.page
            )

        ProductDetailsLoaded (Ok details) ->
            let
                enoughProducts =
                    Dict.size (filterOnlyMissingBarcodes updatedProducts) > 10

                newPage =
                    if enoughProducts then
                        model.page

                    else
                        model.page + 1

                updatedProducts =
                    addProductDetails details model.products
            in
            if isProductDetailsLoaded updatedProducts then
                ( { model
                    | products = updatedProducts
                    , page = newPage
                    , loadingReport = not enoughProducts
                  }
                , if enoughProducts then
                    loadRemainsForReport model.token updatedProducts

                  else
                    loadReport model.token newPage
                )

            else
                ( { model
                    | products = updatedProducts
                  }
                , Cmd.none
                )

        ProductDetailsLoaded (Err err) ->
            ( { model | err = Just err }
            , Cmd.none
            )

        ReportLoaded (Ok report) ->
            ( { model
                | products = Dict.union report model.products
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

        LoadedRemains (Ok remainsInfos) ->
            ( { model | products = updateRemains remainsInfos model.products }, Cmd.none )

        LoadedRemains (Err err) ->
            ( { model | err = Just err }, Cmd.none )


isProductDetailsLoaded : Report -> Bool
isProductDetailsLoaded report =
    List.all isJust (List.map .details (Dict.values report))


updateRemains : Dict String RemainsInfo -> Report -> Report
updateRemains remainsInfos report =
    Dict.map (\code value -> updateProductWithQuantity value (Dict.get code remainsInfos)) report


updateProductWithQuantity : ReportRow -> Maybe RemainsInfo -> ReportRow
updateProductWithQuantity row remainsInfo =
    case remainsInfo of
        Just info ->
            { row | quantity = Just info.quantity }

        Nothing ->
            row


updateBarcode : String -> Maybe ProductDetails -> Cmd Msg
updateBarcode token productDetails =
    case productDetails of
        Just val ->
            sendBarcodeUpdate token val

        Nothing ->
            Cmd.none


type alias RemainsInfos =
    Dict String RemainsInfo


loadRemainsForReport : String -> Report -> Cmd Msg
loadRemainsForReport token report =
    let
        visibleProducts =
            filterVisibleProducts report

        productIds =
            values (Dict.values (Dict.map (\_ row -> Maybe.map .id row.details) visibleProducts))
    in
    prepareRemainsRequest token productIds


prepareRemainsRequest : String -> List String -> Cmd Msg
prepareRemainsRequest token productIds =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = reportRemains productIds
        , body = Http.emptyBody
        , expect = Http.expectJson LoadedRemains remainsInfoListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


loadDetailsForReport : Model -> Report -> List (Cmd Msg)
loadDetailsForReport model report =
    prepareDetailsRequests model.token report



-- |> List.map (Task.attempt ProductDetailsLoaded)


loadReport : String -> Int -> Cmd Msg
loadReport token page =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = reportSalesByVariant page
        , body = Http.emptyBody
        , expect = Http.expectJson ReportLoaded productCodeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


filterOnlyMissingBarcodes : Report -> Report
filterOnlyMissingBarcodes report =
    Dict.filter rowHasMissingBarcodes report


filterVisibleProducts : Report -> Report
filterVisibleProducts report =
    Dict.filter
        (\code row ->
            (rowHasMissingBarcodes code row
                || rowIsChanged row
            )
                && rowInStock row
        )
        report


rowInStock : ReportRow -> Bool
rowInStock row =
    Maybe.withDefault True (Maybe.map (\quantity -> quantity > 0) row.quantity)


rowIsChanged : ReportRow -> Bool
rowIsChanged row =
    case row.details of
        Just _ ->
            row.changed

        Nothing ->
            False


rowHasMissingBarcodes : String -> ReportRow -> Bool
rowHasMissingBarcodes _ row =
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
    Maybe.map (\details -> { details | barcodes = Just [ barcode ] }) productDetails


updateProductDetails : ProductDetails -> Maybe ReportRow -> Maybe ReportRow
updateProductDetails productDetails reportRow =
    Maybe.map (\row -> { row | details = Just productDetails }) reportRow


prepareDetailsRequests : String -> Report -> List (Cmd Msg)
prepareDetailsRequests token links =
    Dict.values
        (Dict.map
            (\_ value ->
                getDetailsOfProduct token value
            )
            links
        )


getDetailsOfProduct : String -> ReportRow -> Cmd Msg
getDetailsOfProduct token reportRow =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = getProxyUrl [ reportRow.href ] []
        , body = Http.emptyBody
        , expect = Http.expectJson ProductDetailsLoaded barCodeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getProducts : String
getProducts =
    getApiUrl [ "entity", "product" ] Nothing


reportRemains : List String -> String
reportRemains productIds =
    let
        productIdsUrlEncoded =
            List.map (\id -> Url.Builder.string "product.id" id) productIds
    in
    getApiUrl
        [ "report"
        , "stock"
        , "all"
        ]
        (Just
            (List.append
                [ Url.Builder.int "limit" 100
                , Url.Builder.string "stockMode" "all"
                ]
                productIdsUrlEncoded
            )
        )


reportSalesByVariant : Int -> String
reportSalesByVariant page =
    getApiUrl
        [ "report"
        , "sales"
        , "byvariant"
        ]
        (Just [ Url.Builder.int "offset" (page * 25), Url.Builder.int "limit" 50 ])


sendBarcodeUpdate : String -> ProductDetails -> Cmd Msg
sendBarcodeUpdate token details =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Authorization" token
            ]
        , url = getApiUrl [ "entity", "product", details.id ] Nothing
        , body = barCodeDecoderForReal details
        , expect = Http.expectJson BarcodeUpdated barCodeDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- "https://online.moysklad.ru/api/remap/1.1/report/sales/byvariant"


getApiUrl : List String -> Maybe (List QueryParameter) -> String
getApiUrl parameters queryParemeters =
    getProxyUrl (List.append [ "https://online.moysklad.ru", "api", "remap", "1.1" ] parameters) (Maybe.withDefault [] queryParemeters)


getProxyUrl : List String -> List QueryParameter -> String
getProxyUrl =
    crossOrigin "https://secure-fjord-80419.herokuapp.com"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


missingBarcodes : Maybe (List String) -> Bool
missingBarcodes barcodes =
    case barcodes of
        Nothing ->
            True

        Just value ->
            List.all isGeneratedBarcode value


isGeneratedBarcode : String -> Bool
isGeneratedBarcode barcode =
    String.startsWith "2000" barcode


renderProduct : ReportRow -> Html Msg
renderProduct product =
    case product.details of
        Just productDetails ->
            let
                barcode =
                    Maybe.withDefault "" (Maybe.andThen List.head productDetails.barcodes)

                color =
                    if product.changed && product.saved then
                        "lightgreen"

                    else if product.changed then
                        "lightyellow"

                    else
                        "white"
            in
            li [ class "item mdl-card mdl-shadow--2dp", style "background" color ]
                [ div [ class "mdl-card__title" ]
                    [ span [ class "mdl-card__title-text item__name" ] [ text productDetails.name ]
                    , span [ class "item__quantity" ] [ text ("Склад: " ++ Maybe.withDefault "_" (Maybe.map String.fromInt product.quantity)) ]
                    ]
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


showError : String -> Html Msg
showError error =
    div [ class "error" ]
        [ div [ class "error__content" ]
            [ p [ class "error__message" ] [ text error ] ]
        ]


renderLogin : Model -> Html Msg
renderLogin model =
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
                , value <|
                    if model.loginInProgress then
                        "Загружается..."

                    else
                        "Войти"
                , disabled model.loginInProgress
                , class "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent mdl-js-ripple-effect"
                ]
                []
            ]
        ]


renderMain : Model -> Html Msg
renderMain model =
    if not model.loggedIn then
        renderLogin model

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
            , Html.main_ [] [ renderListOrLoading (isProductDetailsLoaded model.products && not model.loadingReport) (filterVisibleProducts model.products) ]
            ]


view : Model -> Html Msg
view model =
    case model.err of
        Nothing ->
            div []
                [ Maybe.withDefault (text "") (Maybe.map showError model.errorMessage)
                , renderMain model
                ]

        Just (Http.BadUrl value) ->
            showError ("Неправильная ссылка " ++ value)

        Just Http.Timeout ->
            showError "Превышено время ожидания"

        Just Http.NetworkError ->
            showError "Пропала связь. Попробуйте еще раз."

        Just (Http.BadStatus response) ->
            showError ("Неправильный ответ от сервера:" ++ String.fromInt response)

        Just (Http.BadBody error) ->
            showError ("Ошибка при обработке запроса:" ++ error)


renderListOrLoading : Bool -> Report -> Html Msg
renderListOrLoading allDetailsLoaded report =
    if not allDetailsLoaded then
        div [ class "loading" ]
            [ span [] [ text "Загружается..." ] ]

    else
        ul [ class "items" ] (List.map renderProduct (Dict.values report))


buildAuthorizationToken : String -> String -> String
buildAuthorizationToken username password =
    Base64.encode (username ++ ":" ++ password)
