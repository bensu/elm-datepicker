import Html exposing (..)
import Signal exposing (Message)
import StartApp.Simple exposing (start)
import List as List

import Widget.Datepicker as Datepicker exposing (Action, view, Datepicker)

main =
  start
    { model = { isOn = False, date = { day=0, month=0, year=0 } } 
    , update = Datepicker.update
    , view = Datepicker.view
    }
