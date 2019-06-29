module Main exposing (Model, init, main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Result exposing (..)
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


type alias ErrorMessage =
    Maybe String


type alias Model =
    { input : String, persons : List Person, errorMessage : ErrorMessage }


defaultModel : Model
defaultModel =
    { input = ""
    , persons = [ defaultPerson, defaultPerson ]
    , errorMessage = Nothing
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( defaultModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewErrorMessage model.errorMessage
        , Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button [ disabled (canSubmit model) ] [ text "submit" ]
            ]
        , ul [] (List.map viewPerson model.persons)
        ]


emptyRule : Rule
emptyRule model =
    if model.input |> String.isEmpty then
        Ok ""

    else
        Err "empty"


duplicatedRule : Rule
duplicatedRule model =
    if model.persons |> duplicatedEntry model.input then
        Ok ""

    else
        Err ""


type alias Rule =
    Model -> Result String String


rules : List Rule
rules =
    [ duplicatedRule, emptyRule ]


isOk : Result String String -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


canSubmit : Model -> Bool
canSubmit model =
    List.foldl (||) False (List.map (\func -> (model |> func) |> isOk) rules)


validate : Bool
validate =
    True


duplicatedEntry : String -> List Person -> Bool
duplicatedEntry name persons =
    List.any (\x -> x.name == name) persons


viewErrorMessage : ErrorMessage -> Html msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just m ->
            div [] [ text m ]

        Nothing ->
            div [] []


viewPerson : Person -> Html msg
viewPerson person =
    li [] [ text (person.name ++ ":" ++ (person.age |> Maybe.withDefault 0 |> String.fromInt)) ]


type Msg
    = Input String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            if {model|input = str} |> duplicatedRule |> isOk then
                ( { model | input = str, errorMessage = Just "すでに登録されている" }, Cmd.none )

            else
                ( { model | input = str, errorMessage = Nothing }, Cmd.none )

        Submit ->
            ( { model | input = "", persons = { defaultPerson | name = model.input } :: model.persons }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
