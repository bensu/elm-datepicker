import Html exposing (..)
import Signal exposing (Message)
import StartApp.Simple exposing (start)
import List as List

import Widget.Datepicker as Datepicker exposing (Action, view, Datepicker)

main =
  start
    { model = Datepicker.initValue
    , update = Datepicker.update
    , view = Datepicker.view
    }
