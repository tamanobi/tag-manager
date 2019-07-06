module Main exposing (Model, init, main, view)

import Browser
import Category exposing (Category)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
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


type Page
    = Top
    | Tag String


type alias CategoryModel =
    { input : String
    , name : String
    , labels : List String
    }


type alias Link =
    { tag : String
    , categoryName : String
    , label : String
    }


type alias Model =
    { page : Page
    , inputMode : InputMode
    , input : String
    , errorMessage : List String
    , inputTag : String
    , tags : List String
    , inputCategory : String
    , categories : List CategoryModel
    , links : List Link
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
    | InputTag String
    | AddTag
    | DeleteTag String
    | ChangePage Page
    | GetTags
    | GotTags (Result Http.Error String)


defaultModel : Model
defaultModel =
    { page = Top
    , inputMode = Single
    , input = ""
    , errorMessage = []
    , inputTag = ""
    , tags = [ "鹿目まどか", "佐倉杏子", "能美クドリャフカ" ]
    , inputCategory = ""
    , categories = [ defaultCategoryModel, { input = "", name = "ジャンル", labels = [ "美少女", "ファンタジー" ] } ]
    , links = [ { tag = "鹿目まどか", categoryName = "作品名", label = "魔法少女まどか☆マギカ" } ]
    }


defaultCategoryModel =
    { input = ""
    , name = "作品名"
    , labels = [ "魔法少女まどか☆マギカ", "リトルバスターズ！" ]
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( defaultModel, Cmd.none )


view : Model -> Html Msg
view model =
    case model.page of
        Top ->
            twoColumn <| ( viewTopPage model, div [] [ button [ onClick GetTags ] [ text "APIリクエスト" ] ] )

        Tag s ->
            twoColumn <|
                ( div []
                    [ h1 [] [ text <| "タグ編集画面(" ++ s ++ ")" ]
                    , h2 [] [ text "結びつけるカテゴリを選ぶ" ]
                    , h3 [] [ text "あとでリネーム機能をつける" ]
                    , h3 [] [ text "カテゴリ一覧" ]
                    , table []
                        [ thead []
                            [ tr []
                                [ td [] [ text "check" ]
                                , td [] [ text "yyyyy" ]
                                ]
                            ]
                        , tbody []
                            [ tr []
                                [ td [] [ input [ type_ "checkbox", checked True ] [] ]
                                , td [] [ text "dddd" ]
                                ]
                            ]
                        ]
                    ]
                , div []
                    [ button
                        [ onClick <|
                            ChangePage <|
                                Top
                        ]
                        [ text "back to top" ]
                    ]
                )


twoColumn : ( Html Msg, Html Msg ) -> Html Msg
twoColumn ( left, right ) =
    div [ class "wrap" ]
        [ div [ class "left-column" ] [ left ]
        , div [ class "right-column" ] [ right ]
        ]


viewTopPage : Model -> Html Msg
viewTopPage model =
    div []
        [ viewErrorMessage model.errorMessage
        , viewTags model
        , viewCategories model
        , viewLinks model
        ]


viewLinks : Model -> Html Msg
viewLinks model =
    div []
        [ table [] <|
            th [] [ text "リンク" ]
                :: (model.links
                        |> List.map (\link -> text (link.tag ++ " -> " ++ link.label ++ "@" ++ link.categoryName))
                        |> List.map (\x -> tr [] [ td [] [ x ] ])
                   )
        ]


addingForm : String -> (String -> Msg) -> Msg -> Html Msg
addingForm bind inputMsg clickMsg =
    li [] <|
        inputTextAndButton
            bind
            inputMsg
            clickMsg


inputTextAndButton : String -> (String -> Msg) -> Msg -> List (Html Msg)
inputTextAndButton bind inputMsg clickMsg =
    [ input [ value bind, onInput inputMsg, type_ "text" ] []
    , button [ onClick clickMsg ] [ text "add" ]
    ]


viewCategory : CategoryModel -> Html Msg
viewCategory category =
    let
        display { name, labels } =
            String.concat <|
                [ category.name
                , "("
                , String.fromInt (List.length category.labels)
                , ")"
                ]

        list =
            List.map
                (\label ->
                    li []
                        [ text label
                        , button [ onClick (DeleteCategoryLabel category label) ] [ text "x" ]
                        ]
                )
                category.labels
    in
    div []
        [ h1 []
            [ text <| display category ]
        , div [] <| inputTextAndButton category.input (InputCategoryLabel category) (AddCategoryLabel category)
        , table []
            [ thead [] [ td [] [ text "name" ], td [] [ text "action" ] ]
            , tbody [] <|
                List.map
                    (\label ->
                        tr []
                            [ td [] [ text label ]
                            , td [] [ button [ onClick <| DeleteCategoryLabel category label ] [ text "delete" ] ]
                            ]
                    )
                    category.labels
            ]
        ]


viewTags : Model -> Html Msg
viewTags { inputTag, tags } =
    div []
        [ h1 [] [ text "タグ一覧" ]
        , div [] <| inputTextAndButton inputTag InputTag AddTag
        , table []
            [ thead []
                [ tr []
                    [ td [] [ text "tag" ]
                    , td [] [ text "edit" ]
                    , td [] [ text "delete" ]
                    ]
                ]
            , tbody [] <|
                List.map
                    (\tag ->
                        tr [] <|
                            [ td [] [ a [ onClick <| ChangePage <| Tag tag ] [ text tag ] ]
                            , td [] [ button [ onClick <| ChangePage <| Tag tag ] [ text "edit" ] ]
                            , td [] [ button [ onClick <| DeleteTag tag ] [ text "delete" ] ]
                            ]
                    )
                    tags
            ]
        ]


viewCategories : Model -> Html Msg
viewCategories { inputCategory, categories } =
    div []
        [ h1 [] [ text "カテゴリ一覧" ]
        , div [] <| inputTextAndButton inputCategory InputCategory AddCategory
        , table []
            [ thead []
                [ tr []
                    [ td [] [ text "tag" ]
                    , td [] [ text "edit" ]
                    , td [] [ text "delete" ]
                    ]
                ]
            , tbody [] <|
                List.map
                    (\category ->
                        tr [] <|
                            [ td [] [ text <| category.name ++ (List.length category.labels |> String.fromInt) ]
                            , td [] [ button [] [ text "edit" ] ]
                            , td [] [ button [] [ text "delete" ] ]
                            ]
                    )
                    categories
            ]
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
                , input
                    [ value model.input
                    , onInput Input
                    , type_ "text"
                    ]
                    []
                , button
                    [ disabled (not (canSubmit model))
                    , onClick Submit
                    ]
                    [ text "submit" ]
                ]


viewChangingMode : Model -> Html Msg
viewChangingMode model =
    div []
        [ button
            [ disabled (model.inputMode == Single)
            , onClick (ChangeMode Single)
            ]
            [ text "single" ]
        , button
            [ disabled (model.inputMode == Multiple)
            , onClick (ChangeMode Multiple)
            ]
            [ text "multiple" ]
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
    List.filter (\x -> x /= "") <|
        List.map
            (\x ->
                case x of
                    Ok _ ->
                        ""

                    Err m ->
                        m
            )
            results


canSubmit : Model -> Bool
canSubmit model =
    List.foldl (&&) True <| List.map (\func -> (model |> func) |> isOk) rules


viewErrorMessage : List String -> Html msg
viewErrorMessage messages =
    if messages |> List.isEmpty then
        div [] []

    else
        div [] <| List.map (\x -> text x) messages


type alias Tag =
    String


type alias RawMultipleInput =
    String


toTags : RawMultipleInput -> List Tag
toTags s =
    List.filter (\x -> String.isEmpty x) <| String.split "\n" s


removeByName : String -> List { b | name : String } -> List { b | name : String }
removeByName name list =
    List.filter (\v -> v.name /= name) list


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
            ( { model
                | input = str
                , errorMessage = getErrors <| List.map (\func -> new |> func) rules
              }
            , Cmd.none
            )

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

        InputTag tag ->
            ( { model | inputTag = tag }, Cmd.none )

        AddTag ->
            ( { model | tags = model.inputTag :: model.tags }, Cmd.none )

        DeleteTag tag ->
            ( { model | tags = List.filter (\t -> t /= tag) model.tags }, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        GetTags ->
            ( model
            , Http.get
                { url = "http://localhost:3000/tags"
                , expect = Http.expectString GotTags
                }
            )

        GotTags result ->
            case result of
                Ok str ->
                    ( { model | errorMessage = [ str ] }, Cmd.none )

                Err _ ->
                    ( { model | errorMessage = [ "http error" ] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none