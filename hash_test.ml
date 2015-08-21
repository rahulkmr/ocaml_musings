let () = 
    let hash = Myhash.create () in
    Myhash.add hash "hello" "world";
    Myhash.add hash "world" "hello";
    Myhash.add hash "foo" "bar";
    Myhash.add hash "bar" "foo";
    Printf.printf "%s\n" (Myhash.find hash "world");
    Printf.printf "%s\n" (Myhash.find hash "hello")
