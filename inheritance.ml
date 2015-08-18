class animal species =
    object
        method eat = Printf.printf "A %s eats.\n" species
    end

class pet ~species ~owner ~name =
    object
        inherit animal species
        method owner : string = owner
        method name : string = name
    end

class pet_dog ~owner ~name =
    object (self : 'self)
        inherit pet ~species:"dog" ~owner ~name as super
        method speak = Printf.printf "%s barks!\n" name
        method prepare_to_eat =
            Printf.printf "%s growls.\n" name
        method eat =
            self#prepare_to_eat;
            super#eat
    end

let _ =
    let clifford = new pet_dog ~name:"Clifford" ~owner:"Emily" in
    clifford#speak;
    clifford#eat;
