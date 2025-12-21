module BaseStack = struct
  type 'a t = 'a list
  let empty = []
end

module Stack = struct
  include BaseStack
  let push x stack = x :: stack
  let pop = function
    | [] -> None
    | x :: xs -> Some (x, xs)
end