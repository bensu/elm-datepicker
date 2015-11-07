import Html exposing (..)
import Signal exposing (Message)
import StartApp.Simple exposing (start)
import List as List

import Widget.Accordion as Accordion exposing (Accordion, view)

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

viewPanel : Entry -> Html
viewPanel entry =
  div []
      [Html.text entry.synopsis]

accordion : Signal.Address Action -> Accordion Entry
accordion address =
    { viewHeader = .title >> Html.text
    , viewPanel = viewPanel 
    , setExpanded = \expanded {id} -> Signal.message address (SetExpanded id expanded)
    , getExpanded = .expanded
    }

view : Signal.Address Action -> List Entry -> Html
view address model =
    Accordion.view (accordion address) model

main =
  start
    { model = samples 
    , update = update
    , view = view
    }
