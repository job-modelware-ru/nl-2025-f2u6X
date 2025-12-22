let imperative_sum () =
  let total = ref 0 in
  for i = 1 to 10 do
    total := !total + i        
  done;
  !total


let functional_sum () =
  List.init 10 (fun i -> i + 1)
  |> List.fold_left ( + )
