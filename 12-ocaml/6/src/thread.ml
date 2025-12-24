let thread_fun () =
  for i = 1 to 5 do
    print_endline ("Thread: " ^ string_of_int i);
    Thread.delay 0.5
  done

let () =
  let t = Thread.create thread_fun () in
  thread_fun ();
  Thread.join t