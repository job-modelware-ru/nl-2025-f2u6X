open QCheck

let add_is_commutative =
  Test.make ~count:10_000
    (pair int int)
    (fun (x, y) -> x + y = y + x)