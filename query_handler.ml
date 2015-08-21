open Core.Std

module type Query_handler = sig
    type config with sexp
    val name : string
    type t
    val create : config -> t
    val eval : t -> Sexp.t -> Sexp.t Or_error.t
end


module Unique = struct
    type config = int with sexp
    type t = { mutable next_id : int }

    let name = "unique"
    let create start_at = { next_id = start_at }

    let eval t sexp =
        match Or_error.try_with (fun () -> unit_of_sexp sexp) with
        | Error _ as err -> err
        | Ok () ->
                let response = Ok (Int.sexp_of_t t.next_id) in
                t.next_id <- t.next_id + 1;
                response
end


module List_dir = struct
    type config = string with sexp
    type t = { cwd : string }

    let is_abs p =
        String.length p > 0 && p.[0] = '/'

    let name = "ls"

    let create cwd = { cwd }

    let eval t sexp =
        match Or_error.try_with (fun () -> string_of_sexp sexp) with
        | Error _ as err -> err
        | Ok dir ->
                let dir =
                    if is_abs dir then dir
                    else Filename.concat t.cwd dir
                in
                Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end


module type Query_handler_instance = sig
    module Query_handler : Query_handler
    val this : Query_handler.t
end


let build_instance (type a) (module Q : Query_handler with type config = a) config =
    (module struct
        module Query_handler = Q
            let this = Q.create config
    end : Query_handler_instance)


let build_dispatch_table handlers =
    let table = String.Table.create () in
    List.iter handlers
    ~f:(fun ((module I : Query_handler_instance) as instance) ->
        Hashtbl.replace table ~key:I.Query_handler.name ~data:instance);
        table


let dispatch dispatch_table name_and_query =
    match name_and_query with
    | Sexp.List [Sexp.Atom name; query] ->
            begin match Hashtbl.find dispatch_table name with
            | None -> Or_error.error "Could not find handler." name String.sexp_of_t
            | Some (module I : Query_handler_instance) ->
                    I.Query_handler.eval I.this query
            end
    | _ -> Or_error.error_string "malformed query"



let rec cli dispatch_table =
    printf ">>> %!";
    let result =
        match In_channel.input_line stdin with
        | None -> `Stop
        | Some line ->
                match Or_error.try_with (fun () -> Sexp.of_string line) with
                | Error e -> `Continue (Error.to_string_hum e)
                | Ok (Sexp.Atom "quit") -> `Stop
                | Ok query ->
                        begin match dispatch dispatch_table query with
                        | Error e -> `Continue (Error.to_string_hum e)
                        | Ok s -> `Continue (Sexp.to_string_hum s)
                        end
    in
    match result with
    | `Stop -> ()
    | `Continue msg ->
            printf "%s\n%!" msg;
            cli dispatch_table

let () =
    let unique_instance = build_instance (module Unique) 0 in
    let list_dir_instance = build_instance (module List_dir) "/var" in
    cli (build_dispatch_table [unique_instance; list_dir_instance])
