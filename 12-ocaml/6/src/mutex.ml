let m = Mutex.create ()

let critical () =
  Mutex.lock m;
  print_endline "critical";
  Mutex.unlock m