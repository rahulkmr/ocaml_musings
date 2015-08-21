open Core.Std

let () =
    let buf = In_channel.read_all "book.json" in
    let json1 = Yojson.Basic.from_string buf in
    let json2 = Yojson.Basic.from_file "book.json" in
    print_endline (if json1 = json2 then "OK" else "FAIL")
