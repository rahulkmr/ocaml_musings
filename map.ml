module type ValueSig = sig
    type value
end

module type EqualSig = sig
    type t
    val equal: t -> t -> bool
end

module type SetSig = sig
    type t
    type elt
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val find : elt -> t -> elt
end

module type MapSig = sig
    type t
    type key
    type value
    val empty : t
    val add : t -> key -> value -> t
    val find : t -> key -> value
end

module MakeSet (Equal : EqualSig)
: SetSig with type elt = Equal.t
= struct
    type elt = Equal.t
    type t = elt list
    let empty = []
    let mem x s = List.exists (Equal.equal x) s
    let add x s = x :: s
    let find x s = List.find (Equal.equal x) s
end


module MakeMap (Equal : EqualSig) (Value : ValueSig)
: MapSig
with type key = Equal.t
with type value = Value.value
= struct
    type key = Equal.t
    type value = Value.value
    type item =
        | Key of key
        | Pair of key * value

    module EqualItem = struct
        type t = item
        let equal (Key key1 | Pair (key1, _)) (Key key2 | Pair (key2, _)) =
            Equal.equal key1 key2
    end
    module Set = MakeSet (EqualItem)
    type t = Set.t

    let empty = Set.empty
    let add map key value =
        Set.add (Pair (key, value)) map
    let find map key =
        match Set.find (Key key) map with
        | Pair (_, value) -> value
        | Key _ -> raise (Invalid_argument "find")
end

module StringEqual : EqualSig = struct
    type t = string
    let equal a b = String.lowercase a = String.lowercase b
end

module Value : ValueSig = struct
    type value = string
end

module Map = MakeMap (StringEqual) (Value)
