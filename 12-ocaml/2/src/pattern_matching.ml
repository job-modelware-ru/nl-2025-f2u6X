let x = 3;;
match x with
| 0 -> "ноль"
| 1 -> "один"
| _ -> "другое";;
(* Результат: string = "другое" *)

let rec sum lst =
match lst with
| [] -> 0
| x :: xs -> x + sum xs;;
sum [1; 2; 3];; (* int = 6 *)
