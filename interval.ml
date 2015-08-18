open Core.Std

module type IntervalSig = sig
    type t
    type endpoint
    val create : endpoint -> endpoint -> t
    val is_empty : t -> bool
    val contains : t -> endpoint -> bool
    val intersect : t -> t -> t
end

module type IntervalSig_with_sexp = sig
    include IntervalSig
    include Sexpable with type t := t
end


module Make_interval (Endpoint : sig
type t
include Comparable with type t := t
include Sexpable with type t := t
end)
: (IntervalSig_with_sexp with type endpoint := Endpoint.t)
= struct
    type t =
        | Interval of Endpoint.t * Endpoint.t
        | Empty
    with sexp

    let create low high =
        if Endpoint.compare low high > 0 then Empty
        else Interval (low, high)

    let t_of_sexp sexp =
        match t_of_sexp sexp with
        | Empty -> Empty
        | Interval (x, y) -> create x y

    let is_empty = function
        | Empty -> true
        | Interval _ -> false

    let contains t x =
        match t with
        | Empty -> false
        | Interval (low, high) ->
                Endpoint.compare x low >= 0 && Endpoint.compare x high <= 0

    let intersect t1 t2 =
        let min x y = if Endpoint.compare x y <= 0 then x else y in
        let max x y = if Endpoint.compare x y >= 0 then x else y in
        match t1, t2 with
        | Empty, _ | _, Empty -> Empty
        | Interval (l1, h1), Interval (l2, h2) ->
                create (max l1 l2) (min h1 h2)
end

module Int_interval = Make_interval(Int)

let () =
    let interval1 = Int_interval.create 1 10 in
    let interval2 = Int_interval.create 5 15 in
    let intersection = Int_interval.intersect interval1 interval2 in
    printf "%B\n" (Int_interval.contains interval1 5);
    printf "%B\n" (Int_interval.contains intersection 5);
