let person = ("Forthey", 21, true);;
person;; (* Тип (string * int * bool) *)

let (x, y) = (10, 20);;
print_int x;; (* 10 *)
print_int y;; (* 20 *)

let nums = [1; 2; 3];;
let words = ["OCaml"; "is"];;

List.map (fun x -> x * 2) nums;; (* int list = [2; 4; 6] *)

0 :: !nums;; (* int list = [0; 1; 2; 3] *)

let words_end = ["fun"];;
words @ words_end;; (* string list = ["OCaml"; "is"; "fun"] *)
