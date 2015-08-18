type ordering = Smaller | Equal | Larger

class ['key, 'value] map (compare : 'key -> 'key -> ordering) =
    let equal key1 (key2, _) = compare key1 key2 = Equal in
    object (self : 'self)
        val elements : ('key * 'value) list = []
        method add key value = {< elements = (key, value) :: elements >}
        method find key = snd (List.find (equal key) elements)
    end

let comparer a b =
    if a > b then
        Larger
    else if a < b then
        Smaller
    else
        Equal

let _ =
    let my_map = new map comparer in
    let modified = my_map#add "hello" "world" in
    Printf.printf "%s\n" (modified#find "hello");

    let my_map = new map comparer in
    let modified = my_map#add "hello" 1 in
    Printf.printf "%d\n" (modified#find "hello");

