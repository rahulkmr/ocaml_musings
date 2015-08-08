# load "graphics.cma"

let poly =
    object
        val vertices = [|(46, 70); (54, 70); (60, 150); (40, 150)|]
        method draw = Graphics.fill_poly vertices
    end

let circle =
    object
        val center = (50, 50)
        val radius = 10
        method draw =
            let x, y = center in
            Graphics.fill_circle x y radius
    end

let _ =
    Graphics.open_graph " 200x200";
    poly#draw;
    circle#draw;
    ignore (Graphics.wait_next_event [Graphics.Button_down])
