open Core.Std

let checksum_from_string buf =
    Cryptokit.hash_string (Cryptokit.Hash.md5 ()) buf
    |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
    |> print_endline

let checksum_from_file filename =
    let chan = match filename with
    | "-" -> In_channel.stdin
    | _ -> In_channel.create ~binary:true filename
    in 
    Cryptokit.hash_channel (Cryptokit.Hash.md5 ()) chan
    |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
    |> print_endline

let command =
    Command.basic
        ~summary:"Generate md5 from string or file"
        Command.Spec.(
            empty
            +> flag "-s" (optional string) ~doc:"string Checksum the given string"
            +> flag "-t" no_arg ~doc:" run a build-in time trial"
            +> anon (maybe_with_default "-" ("filename" %: file))
            )
        (fun use_string trial filename () ->
            match trial with
            | true -> printf "Running time trial\n"
            | false -> begin
                match use_string with
                | Some buf -> checksum_from_string buf
                | None -> checksum_from_file filename
                end
        )

let () = Command.run command
