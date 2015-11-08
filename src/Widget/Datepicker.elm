module Widget.Datepicker (Datepicker, Action, view, update, initValue) where

import String as String
import Date as Date exposing (Date)
import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events as Event 
import Signal exposing (Address)
import Json.Decode exposing (float, (:=))

import Date.Util

-- Model

type alias Datepicker = {
     date: Maybe Date,
     selectMonth: (Date.Month, Int),
     isOn: Bool
}

-- Dec 2015: hack to avoid having selectMonth be a Maybe type
-- Users will never see that date, since it is updated on the first focus
initValue = { date = Nothing
            , isOn = False
            , selectMonth = (,) Date.Dec 2015 }

type Action = NoOp | Blur | Focus (Date.Month, Int) | Select Date | IncMonth

------------
-- Header --

type alias Icon = {
    iconName: String,
    iconClass: String,
    linkName: String
}

-- Html for the Prev and Next buttons
icon : Address Action -> Icon -> Html
icon address i =
 let iconClassList =
               (classList 
                         [ ("ui-corner-all", True)
                         , ("ui-icon", True)
                         , (i.iconClass, True)
                         ])
 in 
    a [ classList [ ("ui-corner-all", True)
                  , (i.linkName, True)
                  ]
      , Event.onMouseOver address IncMonth
      , Html.Attributes.title i.iconName
      ]
      [span [iconClassList]
        [Html.text i.iconName]]

-- the Datepicker's title is the current month, i.e November
title : String -> Html
title name = 
  div [class "ui-datepicker-title"]
      [span [class "ui-datepicker-month"]
        [Html.text name]]

-- Group with the navigation icons and current month
header : Address Action -> (Date.Month, Int) -> Html
header address (m,y) =
  div [class "ui-datepicker-header ui-widget-header ui-helper-clearfix ui-corner-all"] 
  [ icon
      address
      { iconName = "Prev"
      , linkName = "ui-datepicker-prev"
      , iconClass = "ui-icon-circle-triangle-w"
      }
  , icon
      address 
      { iconName = "Next"
      , linkName = "ui-datepicker-next"
      , iconClass = "ui-icon-circle-triangle-e"
      }
  , title ((Date.Util.monthName m) ++ " - " ++ (toString y))
  ] 

----------------------------
-- Days of the Week names --

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
            
  
----------------
-- Day Boxes ---

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

{- The first week of the month may not start on a Sunday,
   completeWeek completes the first days, from Sunday to
   the first day, so that they are aligned in the display --}
completeWeek : List (Maybe Date) -> List (Maybe Date)
completeWeek week =
  if (List.length week == 7)
  then week
  else 
    -- Where is the Maybe monad when you need it?
    -- So close yet so far away...
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

calendar : Address Action -> (Date.Month, Int) -> Html
calendar address selectMonth =
  table [class "ui-datepicker-calendar"]
        [thead []
               [tr []
                   (List.map dayLabel days)]
        ,tbody []
               (List.map (weekRow address)
                         (Date.Util.allWeeksInMonth (fst selectMonth)
                                                    (snd selectMonth)))]

-- Show the current date in the input field
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
      [ input [ class "ui-datepicker-input"
              , Event.onBlur address Blur
              , Event.on
                     "focus"
                     ((:=) "timeStamp" Json.Decode.float)
                     (\t -> let d = Date.fromTime t
                            in
                              Signal.message address (Focus ((Date.month d),
                                                             (Date.year d))))
              , value (renderDate datepicker.date)
              ]
              []
      , (div
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
           [ header address datepicker.selectMonth
           , calendar address datepicker.selectMonth
           ])
      ]

update : Action -> Datepicker -> Datepicker 
update action model =
    case action of
        NoOp ->
            model
        Blur ->
            { model | isOn <- False } 
        Focus newSelectMonth ->
            { model | isOn <- True
            , selectMonth <- newSelectMonth}
        Select date ->
            { model | date <- Just date }
        IncMonth ->
          let (m,y) = model.selectMonth
          in
            { model | selectMonth <- (Date.Util.addMonths (m, y) 1) } 
