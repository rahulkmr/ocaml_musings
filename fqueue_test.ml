open Core.Std

let () =
    let added = Fqueue.enqueue Fqueue.empty 2 in
    match (Fqueue.dequeue added) with
    | Some (x, _) -> printf "%d\n" x
    | None -> printf "None"
