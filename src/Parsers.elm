module Parsers exposing (barCodeDecoder, barCodeDecoderForReal, productCodeDecoder, remainsInfoListDecoder)

import Dict exposing (Dict, fromList)
import Http exposing (Body)
import Json.Decode as D exposing (Decoder, field, list, map, map2, map4, map6, string, succeed)
import Json.Encode as E
import Model exposing (ProductDetails, RemainsInfo, Report, ReportRow)


productCodeDecoder : Decoder Report
productCodeDecoder =
    field "rows" (list assortmentDecoder)
        |> map (List.map (\row -> ( row.code, row )))
        |> map fromList


assortmentDecoder : Decoder ReportRow
assortmentDecoder =
    field "assortment" reportRowDecoder


remainsInfoListDecoder : Decoder (Dict String RemainsInfo)
remainsInfoListDecoder =
    field "rows" (list remainsInfoDecoder)
        |> map (List.map (\row -> ( row.code, row )))
        |> map Dict.fromList


remainsInfoDecoder : Decoder RemainsInfo
remainsInfoDecoder =
    map2 RemainsInfo
        (field "quantity" D.int)
        (field "code" string)


barCodeDecoder : Decoder ProductDetails
barCodeDecoder =
    map4 ProductDetails
        (field "code" string)
        (D.maybe (field "barcodes" (list string)))
        (field "name" string)
        (field "id" string)


reportRowDecoder : Decoder ReportRow
reportRowDecoder =
    map6 ReportRow
        (field "code" string)
        (field "meta" (field "href" string))
        (succeed Nothing)
        (succeed True)
        (succeed False)
        (succeed Nothing)


barCodeDecoderForReal : ProductDetails -> Http.Body
barCodeDecoderForReal productDetails =
    Http.jsonBody
        (E.object
            [ ( "barcodes", E.list E.string (Maybe.withDefault [] productDetails.barcodes) )
            ]
        )
