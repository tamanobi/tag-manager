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


type InputMode
    = Multiple
    | Single


type alias CategoryModel =
    { input : String
    , name : String
    , labels : List String
    }


type alias Model =
    { inputMode : InputMode
    , input : String
    , errorMessage : List String
    , tags : List String
    , inputCategory : String
    , categories : List CategoryModel
    }


type Msg
    = Input String
    | ChangeMode InputMode
    | Delete String
    | AddMultiple
    | Submit
    | InputCategoryLabel CategoryModel String
    | AddCategoryLabel CategoryModel
    | DeleteCategoryLabel CategoryModel String
    | DeleteCategory CategoryModel
    | InputCategory String
    | AddCategory


defaultModel : Model
defaultModel =
    { inputMode = Single
    , input = ""
    , errorMessage = []
    , tags = []
    , inputCategory = ""
    , categories = [ defaultCategoryModel, { input = "", name = "ジャンル", labels = [ "美少女", "ファンタジー" ] } ]
    }


defaultCategoryModel =
    { input = ""
    , name = "作品名"
    , labels = [ "魔法少女まどか☆マギカ" ]
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( defaultModel, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewInput model
        , viewErrorMessage model.errorMessage
        , viewCategories model
        , div [] (List.map viewCategory model.categories)
        ]



{- categoryを登録する -}


categoryForm : Model -> Html Msg
categoryForm model =
    li []
        [ input [ value model.inputCategory, onInput InputCategory ] []
        , button [ onClick AddCategory ] [ text "add" ]
        ]


viewCategories : Model -> Html Msg
viewCategories model =
    div []
        [ h1 [] [ text "カテゴリ一覧" ]
        , ul [] (categoryForm model :: List.map viewCategorySimple model.categories)
        ]


viewCategorySimple : CategoryModel -> Html Msg
viewCategorySimple category =
    li []
        [ text (category.name ++ "(" ++ String.fromInt (List.length category.labels) ++ ")")
        , button [ onClick (DeleteCategory category) ] [ text "x" ]
        ]


viewCategory : CategoryModel -> Html Msg
viewCategory category =
    let
        adding =
            li []
                [ input
                    [ value category.input
                    , onInput (InputCategoryLabel category)
                    ]
                    []
                , button [ onClick (AddCategoryLabel category) ] [ text "add label" ]
                ]

        list =
            adding
                :: List.map
                    (\label ->
                        li []
                            [ text label
                            , button [ onClick (DeleteCategoryLabel category label) ] [ text "x" ]
                            ]
                    )
                    category.labels
    in
    div []
        [ h1 [] [ text (String.concat [ category.name, "(", String.fromInt (List.length category.labels), ")" ]) ]
        , ul [] list
        ]


viewTags : List String -> Html Msg
viewTags tags =
    div []
        [ h1 [] [ text "Tags" ]
        , ul [] (List.map (\tag -> li [] [ text tag ]) tags)
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


type alias Rule =
    Model -> Result String String


rules : List Rule
rules =
    [ emptyRule ]


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


viewErrorMessage : List String -> Html msg
viewErrorMessage messages =
    if messages |> List.isEmpty then
        div [] []

    else
        div [] (List.map (\x -> text x) messages)


type alias Tag =
    String


type alias RawMultipleInput =
    String


toTags : RawMultipleInput -> List Tag
toTags s =
    List.filter (\x -> String.isEmpty x) (String.split "\n" s)


removeCategoryByName : String -> List CategoryModel -> List CategoryModel
removeCategoryByName name categories =
    List.filter (\categoryModel -> categoryModel.name /= name) categories


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
            ( { model | input = "" }, Cmd.none )

        Delete str ->
            ( model, Cmd.none )

        AddMultiple ->
            ( { model | tags = toTags model.input }, Cmd.none )

        ChangeMode mode ->
            ( { model | inputMode = mode }, Cmd.none )

        InputCategoryLabel category label ->
            let
                new =
                    { category | input = label }

                rest =
                    removeCategoryByName category.name model.categories
            in
            ( { model | categories = new :: rest }, Cmd.none )

        AddCategoryLabel category ->
            let
                new =
                    { category | input = "", labels = category.input :: category.labels }

                rest =
                    removeCategoryByName category.name model.categories
            in
            ( { model | categories = new :: rest }, Cmd.none )

        DeleteCategoryLabel category targetLabel ->
            let
                new =
                    { category | labels = List.filter (\label -> label /= targetLabel) category.labels }

                rest =
                    removeCategoryByName category.name model.categories
            in
            ( { model | categories = new :: rest }, Cmd.none )

        DeleteCategory category ->
            ( { model | categories = removeCategoryByName category.name model.categories }, Cmd.none )

        InputCategory name ->
            ( { model | inputCategory = name }, Cmd.none )

        AddCategory ->
            ( { model | categories = { defaultCategoryModel | name = model.inputCategory } :: model.categories }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
