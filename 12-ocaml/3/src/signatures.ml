module type STACK = sig
  type 'a t  (* абстрактный тип - реализация скрыта *)
  
  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
  val is_empty : 'a t -> bool
end

module Stack : STACK = struct
  type 'a t = 'a list ref  (* реализация скрыта от пользователя *)
  
  let create () = ref []
  let push stack x = stack := x :: !stack
  let pop stack = 
    match !stack with
    | [] -> None
    | x :: xs -> stack := xs; Some x
  let is_empty stack = !stack = []
end