let rec recursive_sum n =
  if n <= 0 then 0
  else n + recursive_sum (n - 1)

let tail_recursive_sum n =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) (acc + n)
  in
  aux n 
