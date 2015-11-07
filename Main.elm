import Html exposing (..)
import Signal exposing (Message)
import StartApp.Simple exposing (start)
import List as List

import Widget.Datepicker as Datepicker exposing (view)

type Action
  = NoOp
  | SetExpanded Int Bool

type alias Entry =
    { id : Int
    , title : String
    , synopsis : String
    , expanded : Bool
    }

sampleEntry : Entry
sampleEntry =
    { id = 1
    , title = "Walden"
    , synopsis = "A student is bitten by a radioactive spider..."
    , expanded = True 
    }

samples = List.map (\n -> { sampleEntry | id <- n }) [2,3,4,5,6]

update : Action -> List Entry -> List Entry
update action model =
    case action of
        NoOp ->
            model
        SetExpanded id expanded ->
            List.map (\entry ->
                if entry.id == id 
                    then { entry | expanded <- expanded }
                    else entry
            ) model

view : Signal.Address Action -> List Entry -> Html
view address model =
    Datepicker.view "as" 

main =
  start
    { model = samples 
    , update = update
    , view = view
    }
