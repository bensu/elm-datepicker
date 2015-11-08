module Widget.Datepicker (Datepicker, Action, view, update, initValue) where

import WebAPI.Date as WebDate
import String as String
import Date as Date exposing (Date)
import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events as Event 
import Signal exposing (Message, Address)
import Json.Decode

-- Model

type alias Datepicker = {
     date: Maybe Date,
     isOn: Bool
}

now = Date.fromTime 1449010800000
initValue = { date = Nothing, isOn = False }

type Action = NoOp | Blur | Focus | Select Date 

icon : String -> String -> String -> Html
icon name linkName iconName =
 let iconClass =
               (classList 
                         [ ("ui-corner-all", True)
                         , ("ui-icon", True)
                         , (iconName, True)
                         ])
 in 
    a [ classList [ ("ui-corner-all", True)
                  , (linkName, True)
                  ]
      , Html.Attributes.title name
      ]
      [span [iconClass]
        [Html.text name]]

title : String -> Html
title name = 
  div [class "ui-datepicker-title"]
      [span [class "ui-datepicker-month"]
        [Html.text name]]

header : Date.Month -> Html
header month =
  div [class "ui-datepicker-header ui-widget-header ui-helper-clearfix ui-corner-all"] 
  [ icon "Prev" "ui-datepicker-prev" "ui-icon-circle-triangle-w"
  , icon "Next" "ui-datepicker-next" "ui-icon-circle-triangle-e"
  , title (monthName month)
  ] 

type alias WeekDay = { name: String, isWeekend: Bool }

days = List.map2 (\n b -> { name=n, isWeekend=b })
                 ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]
                 [True, False, False, False, False, False, True]

dayLabel : WeekDay -> Html
dayLabel day = 
         th [class (if day.isWeekend
                    then "ui-datepicker-week-end"
                    else "")]
            [span [] [Html.text day.name]]
            
monthNamesShort = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
              
monthData : Date.Month -> (Int, String)
monthData m =
  case m of
       Date.Jan -> (0,"January")
       Date.Feb -> (1,"February")
       Date.Mar -> (2,"March")
       Date.Apr -> (3,"April")
       Date.May -> (4,"May")
       Date.Jun -> (5,"June")
       Date.Jul -> (6,"July")
       Date.Aug -> (7,"August")
       Date.Sep -> (8,"September")
       Date.Oct -> (9,"October")
       Date.Nov -> (10,"November")
       Date.Dec -> (11,"December")
                  
monthNumber : Date.Month -> Int
monthNumber m =
  fst (monthData m)

monthName : Date.Month -> String
monthName m =
  snd (monthData m)
      
addDays : Date -> Int -> Date
addDays date n =
  WebDate.offsetTime (toFloat n * 24 * 3600000) date

allDaysInMonth : Date.Month -> Int -> List Date
allDaysInMonth month year = 
  let parts =
        { year = year
        , month = monthNumber month
        , day = 0
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }
      firstDate = WebDate.fromParts WebDate.Local parts
  in                       
    List.filter (\d -> month == Date.month d)
                (List.map (addDays firstDate) [1..32])

addToGroups : List (List Date) -> Date -> List (List Date)
addToGroups groups date =
  case Date.dayOfWeek date of
    Date.Sun -> [date] :: groups 
    _ -> case List.head groups of
           Nothing -> [[date]]
           Just group -> ((List.append group [date]) ::
                          (case (List.tail groups) of
                             Nothing -> []
                             Just restGroups -> restGroups))

groupToWeeks : List Date -> List (List Date) -> List (List Date) 
groupToWeeks daysLeft groups =
  case List.head daysLeft of
    Nothing -> groups
    Just date ->
      let
        groups' = addToGroups groups date      
      in
        case List.tail daysLeft of
          Nothing -> groups'
          Just restOfDays -> groupToWeeks restOfDays groups'
              
allWeeksInMonth : Date.Month -> Int -> List (List Date)
allWeeksInMonth month year = 
  List.reverse (groupToWeeks (allDaysInMonth month year) [[]])
  
monthDays = allWeeksInMonth (Date.month now) (Date.year now) 

dayNumber : Address Action -> Date -> Html
dayNumber address date =
          td []
             [a [ class "ui-state-default"
                , href "#"
                , Event.onMouseDown address (Select date)
                ]
                [Html.text (toString (Date.day date))]]

weekRow : Address Action -> List Date -> Html
weekRow address week = 
        tr []
           (List.map (dayNumber address) week)

calendar : Address Action -> Html
calendar address =
  table [class "ui-datepicker-calendar"]
        [thead []
               [tr []
                   (List.map dayLabel days)]
        ,tbody []
               (List.map (weekRow address) monthDays)]

renderDate : Maybe Date -> String
renderDate date =
  case date of
    Nothing -> ""
    Just d -> 
      let strs = (List.map (\f -> (toString (f d)))
                           [Date.day, \m -> monthNumber (Date.month m), Date.year])
      in 
        String.concat (List.intersperse "/" strs) 
  
view: Address Action -> Datepicker -> Html
view address datepicker =
  div []
      [ input [ Event.onBlur address Blur
              , Event.onFocus address Focus
              , value (renderDate datepicker.date)
              ]
              []
      , div
           [ class "ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all"
           , attribute "aria-live" "polite"
           , style
               [ ("display", (if datepicker.isOn
                              then "block"
                              else "none"))
               , ("position", "absolute")
               , ("z-index", "1")
               ]
           ]
           [ header (Date.month now)
           , calendar address 
           ]]

update : Action -> Datepicker -> Datepicker 
update action model =
    case action of
        NoOp ->
            model
        Blur ->
            { model | isOn <- False } 
        Focus ->
            { model | isOn <- True }
        Select date ->
            { model | date <- Just date }
