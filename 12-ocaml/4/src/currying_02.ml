let super_math_func log_context func x y = 
  Printf.printf "[%s] " log_context;
  let result = func x y in
  Printf.printf "Result: %d\n" result;
  result

let add = (super_math_func "Debugging sum") (+)
let result = add 5 6