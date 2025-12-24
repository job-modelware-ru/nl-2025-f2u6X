open Domainslib

let () =
  let pool = Task.setup_pool ~num_additional_domains:3 () in
  let r =
    Task.parallel_for_reduce pool ~start:0 ~finish:1_000_000
      ~body:(fun _ -> 1)
      ~reduce:(+) ~init:0
  in
  Printf.printf "Result = %d\n" r;
  Task.teardown_pool pool
  