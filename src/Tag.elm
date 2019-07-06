module Tag exposing (Tag)


type Tag
    = Valid String
    | Invalid


fromString : String -> Tag
fromString str =
    if str |> String.isEmpty then
        Valid str

    else
        Invalid


isValid : Tag -> Bool
isValid tag =
    case tag of
        Valid s ->
            True

        Invalid ->
            False
