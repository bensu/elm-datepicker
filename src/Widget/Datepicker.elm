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
     selectMonth: Maybe (Date.Month, Int),
     isOn: Bool
}

initValue = { date = Nothing
            , isOn = False
            , selectMonth = Nothing }

type Move = Prev | Next

type Action = NoOp | Blur | Focus (Date.Month, Int)
            | Select Date | DeltaMonth Move

------------
-- Header --

type alias Icon = {
    iconName: String,
    iconClass: String,
    linkName: String
}

-- Html for the Prev and Next buttons
icon : Address Action -> Move -> Html
icon address move =
 let
   i = case move of
         Prev -> { iconName = "Prev"
                 , linkName = "ui-datepicker-prev"
                 , iconClass = "ui-icon-circle-triangle-w"
                 }
         Next -> { iconName = "Next"
                 , linkName = "ui-datepicker-next"
                 , iconClass = "ui-icon-circle-triangle-e"
                 }
   iconClassList =
               (classList 
                         [ ("ui-corner-all", True)
                         , ("ui-icon", True)
                         , (i.iconClass, True)
                         ])
 in 
    a [ classList [ ("ui-corner-all", True)
                  , (i.linkName, True)
                  ]
      , Event.onMouseOver address (DeltaMonth move) 
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
  [ icon address Prev
  , icon address Next 
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

isSelected : Maybe Date -> Date -> Bool
isSelected selected date =
  case selected of
    Nothing -> False
    Just d -> (Date.Util.equalDates d date)

dayNumber : Address Action -> Maybe Date -> Maybe Date -> Html
dayNumber address selected date =
  case date of
    Nothing -> td [] []
    Just d ->
          td []
             [a [ classList [ ("ui-state-default", True)
                            , ("ui-state-highlight", isSelected selected d)
                            ]
                , href "#"
                , Event.onMouseDown address (Select d)
                ]
                [Html.text (toString (Date.day d))]]

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

weekRow : Address Action -> Maybe Date -> List Date -> Html
weekRow address selected week = 
        tr []
           (List.map (dayNumber address selected)
                     (completeWeek (List.map (\d -> Just d) week)))

calendar : Address Action -> Maybe Date -> (Date.Month, Int) -> Html
calendar address selected (m,y) =
  table [class "ui-datepicker-calendar"]
        [thead []
               [tr []
                   (List.map dayLabel days)]
        ,tbody []
               (List.map (weekRow address selected)
                         (Date.Util.allWeeksInMonth m y))]

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
  
{- onBlur should be 'click out' to allow for clicks an not onMouseOver
   on other events -}
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
      , case datepicker.selectMonth of
          Nothing -> (div [] [])
          Just m -> (div
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
                       [ header address m
                       , calendar address datepicker.date m
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
            , selectMonth <- case model.selectMonth of 
                               Nothing -> (Just newSelectMonth)
                               Just m -> Just m }
        Select date ->
            { model | date <- Just date }
        DeltaMonth move ->
          case model.selectMonth of
            Nothing -> model
            Just (m,y) -> 
          (let delta = case move of
                        Prev -> -1
                        Next -> 1
           in
             { model | selectMonth <- Just (Date.Util.addMonths (m, y) delta) }) 
