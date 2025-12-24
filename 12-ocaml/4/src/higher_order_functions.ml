let pow2 x = x * x
let double_f = f x = f (f x)

let result = double_f pow2 2 (* pow2 -> pow4 *)

let give_me_flex() = "flex"

let _function = give_me_flex  (* val _function : uint -> string = <fun> *)
let _result = give_me_flex () (* val _result : string = "flex" *)
