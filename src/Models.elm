module Models exposing (..)

import ColorHelper exposing (hueAndShadeToHex)
import Date exposing (Date)
import Date.Extra.Duration exposing (diffDays)
import Material
import Material.Color as Color


type alias Model =
    { currentDate : Maybe Date
    , categories : List Category
    , mdl : Material.Model
    , start : Date
    , end : Date
    , items : List CalendarItem
    }


type alias CalendarItem =
    { start : Date
    , end : Date
    , text : String
    , category : String
    , duration : Int
    }


type alias Category =
    { name : String
    , selected : Bool
    , logo : String
    , hue : Color.Hue
    , leftColor : Color.Color
    , leftColorHex : String
    , leftColorDark : Color.Color
    , leftColorDarkHex : String
    , itemColor : Color.Color
    }


calendarItem : String -> String -> String -> String -> Result String CalendarItem
calendarItem itemText itemCategory itemStart itemEnd =
    case ( Date.fromString itemStart, Date.fromString itemEnd ) of
        ( Ok startDate, Ok endDate ) ->
            Ok
                { start = startDate
                , end = endDate
                , text = itemText
                , category = itemCategory
                , duration = diffDays endDate startDate
                }

        _ ->
            Err ("date parse problem " ++ itemStart ++ " " ++ itemEnd)


categories : List Category
categories =
    [ { name = "meh.com"
      , logo = "https://s3.amazonaws.com/mediocre-static/mehlogo.png"
      , hue = Color.Green
      }
    , { name = "morningsave.com"
      , logo = "https://s3.amazonaws.com/mediocre-static/morningsavelogo.png"
      , hue = Color.Cyan
      }
    , { name = "checkout.org"
      , logo = "https://s3.amazonaws.com/mediocre-static/checkoutlogo.png"
      , hue = Color.BlueGrey
      }
    , { name = "video"
      , logo = "https://s3.amazonaws.com/mediocre-static/videologo.png"
      , hue = Color.Red
      }
    ]
        |> List.map
            (\c ->
                { name = c.name
                , selected = True
                , logo = c.logo
                , hue = c.hue
                , leftColor = Color.color c.hue Color.S500
                , leftColorHex = hueAndShadeToHex c.hue Color.S500
                , leftColorDark = Color.color c.hue Color.S900
                , leftColorDarkHex = hueAndShadeToHex c.hue Color.S700
                , itemColor = Color.color c.hue Color.S300
                }
            )


testData : List CalendarItem
testData =
    [ calendarItem "Meh Shirt" "meh.com" "7/9/2017" "7/10/2017"
    , calendarItem "Meh's 3rd Birthday" "meh.com" "7/10/2017" "7/11/2017"
    , calendarItem "Bormioli Momenti Wine Glasses (4 Pack Red/White)" "meh.com" "7/11/2017" "7/12/2017"
    , calendarItem "MiGear Extreme X Action Camera" "meh.com" "7/12/2017" "7/13/2017"
    , calendarItem "Riviera Hoverboard" "meh.com" "7/13/2017" "7/14/2017"
    , calendarItem "Quikut 20-Piece Knife Set" "meh.com" "7/14/2017" "7/15/2017"
    , calendarItem "Shark Professional Series Steam Pocket Mop (Refurbished)" "meh.com" "7/15/2017" "7/16/2017"
    , calendarItem "Jewelry by Pacific Pearls" "morningsave.com" "7/9/2017" "7/13/2017"
    , calendarItem "THIS WEEK ON TV" "checkout.org" "7/1/2017" "7/16/2017"
    , calendarItem "TMZ SPORTS DEALS" "checkout.org" "7/9/2017" "7/30/2017"
    , calendarItem "Happy Birthday to Meh" "video" "7/9/2017" "7/10/2017"
    , calendarItem "It's a Meh-rathon" "video" "7/10/2017" "7/11/2017"
    , calendarItem "Garfield Hates Mondays: Maybe It Bothers Me More Than It Should" "video" "7/11/2017" "7/13/2017"
    , calendarItem "Clocks: Mad Ape Den Karaoke" "video" "7/13/2017" "7/15/2017"
    , calendarItem "Possum Head Chronicles, series 03 SUPERCUT" "video" "7/15/2017" "7/16/2017"
    ]
        |> List.filterMap Result.toMaybe


partitionWhile : (a -> Bool) -> List a -> ( List a, List a )
partitionWhile predicate l =
    partitionWhileInternal predicate [] l


partitionWhileInternal : (a -> Bool) -> List a -> List a -> ( List a, List a )
partitionWhileInternal predicate acc l =
    let
        nextItem =
            List.head l
    in
    case nextItem of
        Nothing ->
            ( acc, [] )

        Just n ->
            if predicate n then
                case List.tail l of
                    Nothing ->
                        ( acc ++ l, [] )

                    Just tl ->
                        partitionWhileInternal predicate (acc ++ List.singleton n) tl
            else
                ( acc, l )
