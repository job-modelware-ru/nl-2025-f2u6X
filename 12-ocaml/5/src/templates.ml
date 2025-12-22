class ['a] stack =
  object
    val mutable elements : 'a list = []

  method push x = elements <- x :: elements
  method pop =
    match elements with
    | hd :: tl -> elements <- tl; Some hd
    | []-> None
  method is_empty = elements = []
end