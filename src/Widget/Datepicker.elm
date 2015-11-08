module Widget.Datepicker (Datepicker, Action, view, update, initValue) where

import String as String
import Date as Date exposing (Date)
import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events as Event 
import Signal exposing (Message, Address)
import Json.Decode

import Date.Util

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
  , title (Date.Util.monthName month)
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
            
  
monthDays = Date.Util.allWeeksInMonth (Date.month now) (Date.year now) 

dayNumber : Address Action -> Maybe Date -> Html
dayNumber address date =
  case date of
    Nothing -> td [] []
    Just date ->
          td []
             [a [ class "ui-state-default"
                , href "#"
                , Event.onMouseDown address (Select date)
                ]
                [Html.text (toString (Date.day date))]]

-- Where is the Maybe monad when you need it?
-- So close yet so far away...
completeWeek : List (Maybe Date) -> List (Maybe Date)
completeWeek week =
  if (List.length week == 7)
  then week
  else 
    case List.head week of
      Nothing -> completeWeek (Nothing :: week)
      Just d -> case d of
                  Nothing -> completeWeek (Nothing :: week)
                  Just d -> case Date.dayOfWeek d of
                              Date.Sun -> week
                              _ -> completeWeek (Nothing :: week)

weekRow : Address Action -> List Date -> Html
weekRow address week = 
        tr []
           (List.map (dayNumber address)
                     (completeWeek (List.map (\d -> Just d) week)))

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
      let strs = List.map (\f -> (toString (f d)))
                          [ Date.day
                          , \d -> 1 + (Date.Util.monthNumber (Date.month d))
                          , Date.year
                          ]
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
