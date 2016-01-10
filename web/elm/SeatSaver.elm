module SeatSaver where

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import StartApp
import Effects exposing (Effects, Never)
import Task exposing (Task)
import Json.Decode as Json exposing ((:=))
import Http

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  ul [ class "seats" ] (List.map (seatItem address) model)

seatItem : Signal.Address Action -> Seat -> Html
seatItem address seat =
  let
      occupiedClass =
        if seat.occupied then "occupied" else "available"
  in
    li [ class ("seat " ++ occupiedClass), onClick address (Toggle seat) ] [ text (toString seat.seatNo) ]


-- MODEL

type alias Seat =
  { seatNo: Int
  , occupied: Bool
  }

type alias Model =
  List Seat

init : (Model, Effects Action)
init =
    ([], fetchSeats)

-- UPDATE

type Action = Toggle Seat | SetSeats (Maybe Model)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Toggle seatToToggle ->
      let
          updateSeat model =
            if model.seatNo == seatToToggle.seatNo then
              { model | occupied = not model.occupied }
            else
              model
      in
          (List.map updateSeat model, Effects.none)

    SetSeats seats ->
      let
          newModel = Maybe.withDefault model seats
      in
          (newModel, Effects.none)


-- EFFECTS

fetchSeats : Effects Action
fetchSeats =
  Http.get decodeSets "http://localhost:4000/api/seats"
    |> Task.toMaybe
    |> Task.map SetSeats
    |> Effects.task

decodeSets : Json.Decoder Model
decodeSets =
  let
      seat =
        Json.object2 (\seatNo occupied -> (Seat seatNo occupied))
        ("seatNo" := Json.int)
        ("occupied" := Json.bool)

  in
      Json.at ["data"] (Json.list seat)

