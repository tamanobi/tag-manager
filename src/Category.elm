module Category exposing (Category)


type Category
    = Category { name : String, labels : List String }


size : Category -> Int
size c =
    case c of
        Category r ->
            List.length r.labels


new : { name : String, labels : List String } -> Category
new record =
    Category record


addLabel : String -> Category -> Category
addLabel label category =
    case category of
        Category record ->
            new { record | labels = label :: record.labels }


deleteLabel : String -> Category -> Category
deleteLabel name_ category =
    case category of
        Category record ->
            new
                { record
                    | labels =
                        record.labels |> List.filter (\label -> name_ /= label)
                }


name : Category -> String
name c =
    case c of
        Category r ->
            .name r


rename : String -> Category -> Category
rename n c =
    case c of
        Category r ->
            new { r | name = n }
