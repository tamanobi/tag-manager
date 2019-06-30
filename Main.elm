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


type alias Model =
    { input : String, persons : List Person, errorMessage : List String }


defaultModel : Model
defaultModel =
    { input = ""
    , persons = [ defaultPerson, defaultPerson ]
    , errorMessage = []
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
            , button [ disabled (not (canSubmit model)) ] [ text "submit" ]
            ]
        , ul [] (List.map viewPerson model.persons)
        ]


emptyRule : Rule
emptyRule model =
    if model.input |> String.isEmpty then
        Err "empty"

    else
        Ok ""


duplicatedRule : Rule
duplicatedRule model =
    if model.persons |> duplicatedEntry model.input then
        Err "重複している"

    else
        Ok ""


forbiddenWordRule model =
    if not (String.contains "password" model.input) then
        Ok ""

    else
        Err "許されない文字列"


tooLongRule model =
    if String.length model.input > 4 then
        Err "長過ぎる"

    else
        Ok ""


type alias Rule =
    Model -> Result String String


rules : List Rule
rules =
    [ duplicatedRule, emptyRule, forbiddenWordRule, tooLongRule ]


isOk : Result String String -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


getErrors : List (Result String String) -> List String
getErrors results =
    List.filter (\x -> x /= "")
        (List.map
            (\x ->
                case x of
                    Ok _ ->
                        ""

                    Err m ->
                        m
            )
            results
        )


canSubmit : Model -> Bool
canSubmit model =
    List.foldl (&&) True (List.map (\func -> (model |> func) |> isOk) rules)


duplicatedEntry : String -> List Person -> Bool
duplicatedEntry name persons =
    List.any (\x -> x.name == name) persons


viewErrorMessage : List String -> Html msg
viewErrorMessage messages =
    if messages |> List.isEmpty then
        div [] []

    else
        div [] (List.map (\x -> text x) messages)


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
            let
                new =
                    { model | input = str }
            in
            ( { model | input = str, errorMessage = getErrors (List.map (\func -> new |> func) rules) }, Cmd.none )

        Submit ->
            ( { model | input = "", persons = { defaultPerson | name = model.input } :: model.persons }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
