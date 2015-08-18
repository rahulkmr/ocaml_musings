# load "graphics.cma"

class poly vertices =
    object
        val vertices = vertices
        method draw = Graphics.fill_poly vertices
    end

class regular_poly n radius =
    let () = assert (n > 2) in
    let vertices = Array.create n (0, 0) in
    let step = 6.28 /. float_of_int n in
    let () =
        for i = 0 to n - 1 do
            let theta = float_of_int i *. step in
            let x = int_of_float (cos theta *. radius) in
            let y = int_of_float (sin theta *. radius) in
            vertices.(i) <- (x + 100, y + 100)
        done
    in
    object
        method draw = Graphics.fill_poly vertices
    end

let () =
    Graphics.open_graph " 200x200 ";
    let p1 = new poly [|(46, 70); (54, 70); (60, 150); (40, 150)|] in
    let p2 = new poly [|(40, 40); (60, 40); (60, 60); (40, 60)|] in
    let p3 = new regular_poly 8 100.0 in
    p1#draw;
    p2#draw;
    p3#draw;
    ignore (Graphics.wait_next_event [Graphics.Button_down])

