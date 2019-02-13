module Model exposing(Report, ReportRow, RemainsInfo, ProductDetails)
import Dict exposing (Dict)

type alias Report =
    Dict String ReportRow

type alias ReportRow =
    { code : String
    , href : String
    , details : Maybe ProductDetails
    , saved : Bool
    , changed : Bool
    , quantity : Maybe Int
    }

type alias ProductDetails =
    { code : String
    , barcodes : Maybe (List String)
    , name : String
    , id : String
    }

type alias RemainsInfo =
    { quantity : Int
    , code : String
    }