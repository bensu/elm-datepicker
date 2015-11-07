module Widget.Datepicker (view) where


import Html exposing (..) 
import Html.Attributes exposing (class, classList, attribute, style)
import Html.Events exposing (on)
import Signal exposing (Message)
import Json.Decode

icon : String -> String -> String -> Html
icon name linkName iconName =
 let iconClass =
               (classList 
                         [("ui-corner-all", True)
                         ,("ui-icon", True)
                         ,(iconName, True)
                         ])
 in 
    a [classList [ ("ui-corner-all", True)
                 , (linkName, True)
                 ]]
      [span [iconClass]
        [Html.text name]]

title : String -> Html
title name = 
  div [class "ui-datepicker-title"]
      [span [class "ui-datepicker-month"]
        [Html.text name]]

header =
  div [class "ui-datepicker-header ui-widget-header ui-helper-clearfix ui-corner-all"] 
  [ icon "Prev" "ui-datepicker-prev" "ui-icon-circle-triangle"
  , icon "Next" "ui-datepicker-next" "ui-icon-circle-triangle-e"
  , title "November"
  ] 

type alias Day = { name: String, isWeekend: Bool }

days = List.map2 (\n b -> { name=n, isWeekend=b })
                 ["Su", "Mo", "Tu", "Wed", "Th", "Fr", "Sa"]
                 [True, False, False, False, False, False, True]

dayLabel : Day -> Html
dayLabel day = 
         th [class (if day.isWeekend
                    then "ui-datepicker-week-end"
                    else "")]
            [span [] [Html.text day.name]]
            
dayNumbers = List.repeat 7 1
monthDays = List.repeat 4 dayNumbers

dayNumber : Int -> Html
dayNumber n =
          td [class "ui-datepicker"]
             [a [class "ui-state-default"]
                [Html.text (toString n)]]

weekRow : List Int -> Html
weekRow week = 
        tr []
           (List.map dayNumber week)

calendar : String -> Html
calendar a =
  table [class "ui-datepicker-calendar"]
        [thead []
               [tr []
                   (List.map dayLabel days)]
        ,tbody []
               (List.map weekRow monthDays)]

view : String -> Html
view a =
        div
            [ class "ui-datepicker ui-widget ui-widget-content ui-helper-clearfix ui-corner-all"
            , attribute "aria-live" "polite"
            , style
                [ ("display", "block")
                , ("position", "absolute")
                , ("z-index", "1")
                ]
            ]
            [ header
            , calendar "asdf"]
