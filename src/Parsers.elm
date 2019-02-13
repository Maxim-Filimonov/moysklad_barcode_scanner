module Parsers exposing(remainsInfoListDecoder, productCodeDecoder, barCodeEncoder, barcodeEncoderForReal)
import Model exposing(Report, ReportRow, RemainsInfo, ProductDetails)
import Dict exposing(Dict,fromList)
import Json.Decode as D exposing (field, list, map, map2, map4, map6, string, succeed)
import Json.Encode as E
import Http exposing(Body)

productCodeDecoder : D.Decoder Report
productCodeDecoder =
    field "rows" (list assortmentDecoder)
        |> map (List.map (\row -> ( row.code, row )))
        |> map fromList


assortmentDecoder : D.Decoder ReportRow
assortmentDecoder =
    field "assortment" reportRowDecoder

remainsInfoListDecoder : D.Decoder (Dict String RemainsInfo)
remainsInfoListDecoder =
    field "rows" (list remainsInfoDecoder)
        |> map (List.map (\row -> ( row.code, row )))
        |> map Dict.fromList


remainsInfoDecoder : D.Decoder RemainsInfo
remainsInfoDecoder =
    map2 RemainsInfo
        (field "quantity" D.int)
        (field "code" string)


barCodeEncoder : D.Decoder ProductDetails
barCodeEncoder =
    map4 ProductDetails
        (field "code" string)
        (D.maybe (field "barcodes" (list string)))
        (field "name" string)
        (field "id" string)

reportRowDecoder : D.Decoder ReportRow
reportRowDecoder =
    map6 ReportRow
        (field "code" string)
        (field "meta" (field "href" string))
        (succeed Nothing)
        (succeed True)
        (succeed False)
        (succeed Nothing)

barcodeEncoderForReal : ProductDetails -> Body
barcodeEncoderForReal productDetails =
    Http.jsonBody
        (E.object
            [ ( "barcodes", E.list E.string (Maybe.withDefault [] productDetails.barcodes) )
            ]
        )