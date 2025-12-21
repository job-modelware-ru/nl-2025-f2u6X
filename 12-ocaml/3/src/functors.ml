module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module MakeSet (Elt : ORDERED) = struct
  type elt = Elt.t
  type t = elt list
  
  let empty = []
  
  let add x set =
    if List.exists (fun y -> Elt.compare x y = 0) set
    then set
    else x :: set
end

module IntSet = MakeSet(struct
  type t = int
  let compare = compare
end)