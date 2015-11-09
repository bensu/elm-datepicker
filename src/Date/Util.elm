module Date.Util (equalDates, monthNumber, monthName, allWeeksInMonth, addMonths, newDate) where

import Date as Date exposing (Date)
import WebAPI.Date as WebDate

monthNamesShort = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
              
monthData : Date.Month -> (Int, String)
monthData m =
  case m of
       Date.Jan -> (0,"January")
       Date.Feb -> (1,"February")
       Date.Mar -> (2,"March")
       Date.Apr -> (3,"April")
       Date.May -> (4,"May")
       Date.Jun -> (5,"June")
       Date.Jul -> (6,"July")
       Date.Aug -> (7,"August")
       Date.Sep -> (8,"September")
       Date.Oct -> (9,"October")
       Date.Nov -> (10,"November")
       Date.Dec -> (11,"December")
                  
monthNumber : Date.Month -> Int
monthNumber m =
  fst (monthData m)

monthName : Date.Month -> String
monthName m =
  snd (monthData m)
      
-- add an extra 3 hours to avoid daylight saving time changes
addDays : Date -> Int -> Date
addDays date n =
  WebDate.offsetTime (toFloat ((n * 24 + 3) * 3600000)) date

newDate : Int -> Date.Month -> Date
newDate year month = 
  let parts =
        { year = year
        , month = WebDate.fromMonth month
        , day = 1
        , hour = 0
        , minute = 1
        , second = 0
        , millisecond = 0
        }
  in
    WebDate.fromParts WebDate.Local parts

allDaysInMonth : Date.Month -> Int -> List Date
allDaysInMonth month year = 
  let
    firstDate = newDate year month
  in                       
    List.filter (\d -> month == Date.month d)
                (List.map (addDays firstDate) [0..32])

addToGroups : List (List Date) -> Date -> List (List Date)
addToGroups groups date =
  case Date.dayOfWeek date of
    Date.Sun -> [date] :: groups 
    _ -> case List.head groups of
           Nothing -> [[date]]
           Just group -> ((List.append group [date]) ::
                          (case (List.tail groups) of
                             Nothing -> []
                             Just restGroups -> restGroups))

groupToWeeks : List Date -> List (List Date) -> List (List Date) 
groupToWeeks daysLeft groups =
  case List.head daysLeft of
    Nothing -> groups
    Just date ->
      let
        groups' = addToGroups groups date      
      in
        case List.tail daysLeft of
          Nothing -> groups'
          Just restOfDays -> groupToWeeks restOfDays groups'
              
allWeeksInMonth : Date.Month -> Int -> List (List Date)
allWeeksInMonth month year = 
  List.reverse (groupToWeeks (allDaysInMonth month year) [[]])

addMonths : (Date.Month, Int) -> Int -> (Date.Month, Int)
addMonths (m, y) a =
  let date = newDate y m
      date' = (WebDate.offsetMonth WebDate.Local a date)
  in
    (Date.month date', Date.year date')

equalDates : Date -> Date -> Bool
equalDates d1 d2 =
  (Date.day d1 == Date.day d2) && 
  (Date.month d1 == Date.month d2) &&
  (Date.year d1 == Date.year d2)

