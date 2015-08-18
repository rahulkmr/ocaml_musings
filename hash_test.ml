let () = 
    let hash = Hash.create () in
    Hash.add hash "hello" "world";
    Hash.add hash "world" "hello";
    Hash.add hash "foo" "bar";
    Hash.add hash "bar" "foo";
    Printf.printf "%s\n" (Hash.find hash "world");
    Printf.printf "%s\n" (Hash.find hash "hello")
