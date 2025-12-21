exception My_error of string

let do_stuff x =
  if x < 0 then
    raise (My_error "negative!")
  else
    x + 1

try
  do_stuff (-1)
with
| My_error msg -> Printf.printf "Error: %s\n" msg
| _ -> print_endline "Unknown error"
