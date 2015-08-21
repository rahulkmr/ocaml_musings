type 'a hash_entry
type 'a hash_table

val create : unit -> 'a hash_table
val add : 'a hash_table -> string -> 'a -> unit
val find : 'a hash_table -> string -> 'a
