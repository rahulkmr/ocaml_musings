type comparison = int

class virtual comparable =
    object (self : 'self)
        method virtual compare : 'self -> comparison
        method less_than (x : 'self) = compare self x > 0
    end

class virtual number =
    object (self : 'self)
        method virtual zero : 'self
        method virtual neg : 'self
        method virtual compare : 'self -> comparison
        method abs =
            if self#compare self#zero < 0 then
                self#neg
            else
                self
    end

class float_number x =
    object (self : 'self)
        inherit comparable
        inherit number

        val number = x
        method repr = number
        method zero = {< number = 0.0 >}
        method neg = {< number = -. number >}
        method compare y = Pervasives.compare x y#repr
    end
