module Main exposing (Model, init, main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Person =
    { name : String
    , age : Maybe Int
    }


defaultPerson : Person
defaultPerson =
    { name = "default"
    , age = Nothing
    }


type alias Model =
    { persons : List Person }


init : () -> ( Model, Cmd msg )
init _ =
    ( { persons = [ defaultPerson, defaultPerson ] }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        (List.map
            (\person ->
                div []
                    [ text (person.name ++ ":" ++ (person.age |> Maybe.withDefault 0 |> String.fromInt))
                    ]
            )
            model.persons
        )


type Msg
    = Add
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
