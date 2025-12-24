let fibonacci_imperative n =
  if n <= 1 then n
  else
    let fib_array = Array.make (n + 1) 0 in
    fib_array.(0) <- 0;
    fib_array.(1) <- 1;

    for i = 2 to n do
      fib_array.(i) <- fib_array.(i - 1) + fib_array.(i - 2)
    done;

    fib_array.(n)


let fibonacci_functional n =
  let rec loop counter a b =
    if counter >= n then a
    else loop (counter + 1) b (a + b)
  in
  if n <= 1 then n
  else loop 1 0 1
