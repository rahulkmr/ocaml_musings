open Core.Std

type 'a iterator = < get : 'a; has_value : bool; next : unit >

class ['a] list_iterator init = object
    val mutable current : 'a list = init

    method has_value = current <> []

    method get =
        match current with
        | hd :: _ -> hd
        | [] -> raise (Invalid_argument "no value")

    method next =
        match current with
        | _ :: tl -> current <- tl
        | [] -> raise (Invalid_argument "no value")
end

class ['a] istack init = object
    val mutable v : 'a list = init

    method pop =
        match v with
        | hd :: tl ->
                v <- tl;
                Some hd
        | [] -> None

    method push hd =
        v <- hd :: v

    method iterator : 'a iterator =
        new list_iterator v

    method iter ~f =
        List.iter ~f v

    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
        (fun f init -> List.fold ~f ~init v)
end

class sstack init = object
    inherit [string] istack init

    method print =
        List.iter ~f:print_string v
end

class double_stack init = object
    inherit [int] istack init as super

    method push hd =
        super#push (hd * 2)
end


let () =
    let s = new istack [1;2;3] in
    let it = s#iterator in
    while it#has_value do
        printf "%d\n" it#get;
        it#next
    done;
    s#iter ~f:(printf "%d\n");
    printf "%d\n" (s#fold (+) 0);
