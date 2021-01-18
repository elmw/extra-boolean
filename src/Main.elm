module Main exposing (main)
{-|
A "Hello, World!" in an introductory computer program.
@docs main
-}

import Browser
import Html exposing (Html, div, text, input, button)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput, onClick)



-- MAIN
{-|
Start application with model, update & view (MVC).

  main
-}
main: Program () Model Msg
main = 
  Browser.sandbox {init = init, update = update, view = view}



-- MODEL
type alias Model = {
    name: String,
    say: String
  }

init: Model
init =
  Model "" ""



-- UPDATE
type Msg =
  Name String |
  Say

update : Msg -> Model -> Model
update msg model =
  let {name, say} = model in
  case msg of
    Name x ->
      Model x say
    Say ->
      Model name name



-- VIEW
view : Model -> Html Msg
view model =
  let {name, say} = model in
  div [] [
    div [] [
      input [placeholder "Your name", onInput Name] [],
      button [onClick Say] [text "Say"]
    ],
    div [] [
      text "Hello ",
      text say,
      text "!"
    ]
  ]
