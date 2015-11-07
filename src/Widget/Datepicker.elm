module Widget.Datepicker (Datepicker, Action, view, update) where

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onFocus)
import Signal exposing (Message, Address)
import Json.Decode

-- Model

type alias Datepicker = {
     isOn: Bool
}

type Action = NoOp | Blur | Focus

icon : String -> String -> String -> Html
icon name linkName iconName =
 let iconClass =
               (classList 
                         [("ui-corner-all", True)
                         ,("ui-icon", True)
                         ,(iconName, True)
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

type alias Day = { name: String, isWeekend: Bool }

days = List.map2 (\n b -> { name=n, isWeekend=b })
                 ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]
                 [True, False, False, False, False, False, True]

dayLabel : Day -> Html
dayLabel day = 
         th [class (if day.isWeekend
                    then "ui-datepicker-week-end"
                    else "")]
            [span [] [Html.text day.name]]
            
dayNumbers = List.repeat 7 1
monthDays = List.repeat 4 dayNumbers

monthNamesShort = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]

monthNames = [ "January","February","March","April","May","June",
               "July","August","September","October","November","December" ]
               
dayNumber : Int -> Html
dayNumber n =
          td []
             [a [ class "ui-state-default"
                , href "#"
                ]
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

view: Address Action -> Datepicker -> Html
view address datepicker =
  div []
      [ input [ onBlur address Blur
              , onFocus address Focus]
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
           , calendar "asdf"
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
