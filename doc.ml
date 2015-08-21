open Core.Std

type doc =
    | Heading of string
    | Paragraph of text_item list
    | Definition of string list_item list
and text_item =
    | Raw of string
    | Bold of text_item list
    | Enumerate of int list_item list
    | Quote of doc
and 'a list_item =
    { tag : 'a; text : text_item list }


class ['a] folder = object(self: 'self)
    method doc acc = function
        | Heading _ -> acc
        | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
        | Definition list -> List.fold ~f:self#list_item ~init:acc list

    method list_item: 'b. 'a -> 'b list_item -> 'a =
        fun acc {tag; text} ->
            List.fold ~f:self#text_item ~init:acc text

    method text_item acc = function
        | Raw _ -> acc
        | Bold text -> List.fold ~f:self#text_item ~init:acc text
        | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
        | Quote doc -> self#doc acc doc
end

class counter = object
    inherit [int] folder as super

    method list_item acc li = acc

    method text_item acc ti =
        let acc = super#text_item acc ti in
        match ti with
        | Bold _ -> acc + 1
        | _ -> acc
end


