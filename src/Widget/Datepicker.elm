module Widget.Datepicker (Datepicker, Action, view, update) where

import String as String
import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events as Event 
import Signal exposing (Message, Address)
import Json.Decode

-- Model

type alias Date = {
     day: Int,
     month: Int,
     year: Int
}

type alias Datepicker = {
     date: Date,
     isOn: Bool
}

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

header =
  div [class "ui-datepicker-header ui-widget-header ui-helper-clearfix ui-corner-all"] 
  [ icon "Prev" "ui-datepicker-prev" "ui-icon-circle-triangle-w"
  , icon "Next" "ui-datepicker-next" "ui-icon-circle-triangle-e"
  , title "November"
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
            
dayNumbers = List.map (\n -> { day=n, month=1, year=2015}) (List.repeat 7 1)
monthDays =  (List.repeat 4 dayNumbers)

monthNamesShort = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

monthNames = [ "January","February","March","April","May","June",
               "July","August","September","October","November","December" ]
               
dayNumber : Address Action -> Date -> Html
dayNumber address date =
          td []
             [a [ class "ui-state-default"
                , href "#"
                , Event.onMouseDown address (Select date)
                ]
                [Html.text (toString date.day)]]

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

renderDate : Date -> String
renderDate date =
  let strs = List.map toString [date.day, date.month, date.year]
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
           [ header
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
            { model | date <- date }
