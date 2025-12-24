module Container = struct
  module Stack = struct
    type 'a t = 'a list
    let empty = []
  end
  
  module Queue = struct
    type 'a t = 'a list * 'a list
    let empty = ([], [])
  end
end