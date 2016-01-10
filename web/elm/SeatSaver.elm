module SeatSaver where

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (class)
import StartApp.Simple
import Html.Events exposing (onClick)

main : Signal Html
main =
  StartApp.Simple.start
    { model = init
    , update = update
    , view = view
    }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  ul [ class "seats" ] (List.map (seatItem address) model)

seatItem : Signal.Address Action -> Seat -> Html
seatItem address seat =
  li [ class "seat available", onClick address (Toggle seat) ] [ text (toString seat.seatNo) ]


-- MODEL

type alias Seat =
  { seatNo: Int
  , occupied: Bool
  }

type alias Model =
  List Seat

init : Model
init =
  [ { seatNo = 1, occupied = False }
  , { seatNo = 2, occupied = False }
  , { seatNo = 3, occupied = False }
  , { seatNo = 4, occupied = False }
  , { seatNo = 5, occupied = False }
  , { seatNo = 6, occupied = False }
  , { seatNo = 7, occupied = False }
  , { seatNo = 8, occupied = False }
  , { seatNo = 9, occupied = False }
  , { seatNo = 10, occupied = False }
  , { seatNo = 11, occupied = False }
  , { seatNo = 12, occupied = False }
  ]


-- UPDATE
type Action = Toggle Seat

update : Action -> Model -> Model
update action model =
  case action of
    Toggle seatToToggle ->
      let
          updateSeat model =
            if seatToToggle.seatNo == model.seatNo then
              { model | occupied = (not model.occupied) }
            else
              model
      in
          List.map updateSeat model

