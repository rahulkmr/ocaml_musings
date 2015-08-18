# load "graphics.cma"

let transform =
    object (self : 'self)
        val matrix = (1., 0., 0., 0., 1., 0.)
        method new_scale sx sy =
            {< matrix = (sx, 0., 0., 0., sy, 0.) >}
        method new_rotate theta =
            let s, c = sin theta, cos theta in
            {< matrix = (c, -.s, 0., s, c, 0.) >}
        method new_translate dx dy =
            {< matrix = (1., 0., dx, 0., 1., dy) >}
        method repr = matrix
        method transform (x, y) =
            let (m11, m12, m13, m21, m22, m23) = matrix in
            (m11 *. x +. m12 *. y +. m13,
             m21 *. x +. m22 *. y +. m23)
        method multiply other =
            let (x11, x12, x13, x21, x22, x23) = matrix in
            let (y11, y12, y13, y21, y22, y23) = other#repr in
            {< matrix =
                (x11 *. y11 +. x12 *. y21,
                 x11 *. y12 +. x12 *. y22,
                 x11 *. y13 +. x12 *. y23 +. x13,
                 x21 *. y11 +. x22 *. y21,
                 x21 *. y12 +. x22 *. y22,
                 x21 *. y13 +. x22 *. y23 +. x23)
            >}
    end

let int_coord (x, y) = (int_of_float x, int_of_float y)

let new_poly vertices =
    object
        val vertices = vertices
        method draw = Graphics.fill_poly (Array.map int_coord vertices)
        method transform matrix = {< vertices = Array.map matrix#transform vertices >}
    end

let new_circle (center : float * float) (radius : float) =
    object
        val center = center
        val radius = radius
        method draw =
            let x, y = int_coord center in
            let radius = int_of_float radius in
            Graphics.fill_circle x y radius
        method transform matrix = {< center = matrix#transform center >}
    end

let new_square lower_left width =
    object
        val lower_left = lower_left
        val width = width
        method draw =
            let (x, y) = lower_left in
            Graphics.fill_rect x y width width
        method transform matrix = {< lower_left = matrix#transform lower_left >}
    end

let new_collection () =
    object (self : 'self)
        val mutable items = []
        method add item = items <- item::items
        method add_multiple n matrix item =
            if n > 0 then begin
                self#add item;
                self#add_multiple (n - 1) matrix (item#transform matrix)
            end

        method draw = List.iter (fun item -> item#draw) items
        method transform matrix =
            {< items = List.map (fun item -> item#transform matrix) items >}
    end

let star =
    let poly = new_poly [|(0.0, 0.2); (0.1, 0.5); (0.0, 1.0); (-0.1, 0.5)|] in
    let star = new_collection () in
    star#add (new_circle (0.0, 0.0) 0.1);
    for i = 0 to 9 do
        let trans = transform#new_rotate (0.628 *. (float_of_int i)) in
        star#add (poly#transform trans)
    done;
    star

let starry_night =
    let starry_night = new_collection () in
    let add_star (x, y, scale) =
        let trans = (transform#new_translate x y)#multiply (transform#new_scale scale scale) in
        starry_night#add (star#transform trans) in
    List.iter add_star
    [0.35, 0.50, 0.15;
    0.12, 0.95, 0.12;
    0.35, 0.95, 0.10;
    0.62, 0.90, 0.12;
    0.95, 0.85, 0.20];
    starry_night


let _ =
    Graphics.open_graph " 400x400";
    let poly = new_poly [|(-0.05, 0.2); (0.05, 0.2); (0.1, 1.0); (-0.1, 1.0)|] in
    let circle = new_circle (0.0, 0.0) 0.1 in
    let matrix1 =
        (transform#new_translate 100.0 280.0)#multiply (transform#new_scale 100.0 100.0) in
    for i = 0 to 9 do
        let matrix2 = matrix1#multiply (transform#new_rotate (0.628 *. float_of_int i)) in
        (poly#transform matrix2)#draw
    done;
    (circle#transform matrix1)#draw;
    starry_night#draw;

    let line = new_poly [|(0., 0.); (2., 0.); (2., 30.); (0., 30.)|] in
    let xform = (transform#new_translate 3. 0.)#multiply (transform#new_scale 1.1 1.1) in
    let image = new_collection () in
    image#add_multiple 25 xform line;
    image#draw;

    ignore (Graphics.wait_next_event [Graphics.Button_down])
