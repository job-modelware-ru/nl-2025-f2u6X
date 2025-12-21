let counter = Atomic.make 0

let increment () =
  let rec loop () =
    let old = Atomic.get counter in
    if Atomic.compare_and_set counter old (old + 1)
    then ()
    else loop ()
  in loop ()
  