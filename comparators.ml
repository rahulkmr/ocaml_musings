open Core.Std

module Reverse = Comparator.Make(struct
    type t = string
    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let compare x y = String.compare y x
end)

let () =
    let alist = ["foo", 0; "snoo", 3] in
    let ord_map = Map.of_alist_exn ~comparator:String.comparator alist in
    let rev_map = Map.of_alist_exn ~comparator:Reverse.comparator alist in
    match Map.min_elt ord_map, Map.min_elt rev_map with
    | Some (a, _), Some (b, _) -> printf "%s %s\n" a b
    | Some (a, _), None -> printf "%s\n" a
    | None, Some (b, _) -> printf "%s\n" b
    | None, None -> printf "Not found\n"
