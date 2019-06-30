module Main exposing (Model, init, main, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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


type InputMode
    = Multiple
    | Single


type alias Model =
    { inputMode : InputMode, input : String, persons : List Person, errorMessage : List String, tags : List Tag }


defaultModel : Model
defaultModel =
    { inputMode = Single
    , input = ""
    , persons = [ defaultPerson, defaultPerson ]
    , errorMessage = []
    , tags = []
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( defaultModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewInput model
        , viewErrorMessage model.errorMessage
        , ul [] (List.map viewPerson model.persons)
        , ul [] (List.map (\tag -> li [] [ text tag ]) model.tags)
        ]


viewInput : Model -> Html Msg
viewInput model =
    case model.inputMode of
        Multiple ->
            div []
                [ viewChangingMode model
                , textarea [ value model.input, onInput Input ] []
                , button [ onClick AddMultiple ] [ text "add" ]
                ]

        Single ->
            div []
                [ viewChangingMode model
                , input [ value model.input, onInput Input ] []
                , button [ disabled (not (canSubmit model)), onClick Submit ] [ text "submit" ]
                ]


viewChangingMode : Model -> Html Msg
viewChangingMode model =
    div []
        [ button [ disabled (model.inputMode == Single), onClick (ChangeMode Single) ] [ text "single" ]
        , button [ disabled (model.inputMode == Multiple), onClick (ChangeMode Multiple) ] [ text "multiple" ]
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


type alias Rule =
    Model -> Result String String


rules : List Rule
rules =
    [ duplicatedRule, emptyRule, forbiddenWordRule ]


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


viewPerson : Person -> Html Msg
viewPerson person =
    li []
        [ text (person.name ++ ":" ++ (person.age |> Maybe.withDefault 0 |> String.fromInt))
        , button [ onClick (Delete person) ] [ text "x" ]
        ]


type Msg
    = Input String
    | ChangeMode InputMode
    | Delete Person
    | AddMultiple
    | Submit


deleteByName name persons =
    List.filter (\x -> x.name /= name) persons


type alias Tag =
    String


type alias RawMultipleInput =
    String


toTags : RawMultipleInput -> List Tag
toTags s =
    List.filter (\x -> String.isEmpty x) (String.split "\n" s)


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

        Delete person ->
            ( { model | persons = deleteByName person.name model.persons }, Cmd.none )

        AddMultiple ->
            ( { model | tags = toTags model.input }, Cmd.none )

        ChangeMode mode ->
            ( { model | inputMode = mode }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
