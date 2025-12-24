open Thread

(* Общие ресурсы *)
let mutex = Mutex.create ()
let cond = Condition.create ()
let queue = Queue.create ()

(* Потребитель: ждёт задач *)
let consumer () =
  while true do
    Mutex.lock mutex;

    (* Пока очередь пуста — ждём *)
    while Queue.is_empty queue do
      Condition.wait cond mutex
    done;

    (* Когда проснулся — mutex снова захвачен *)
    let task = Queue.pop queue in
    Mutex.unlock mutex;

    Printf.printf "Consumed: %s\n%!" task
  done

(* Производитель: добавляет задачи *)
let producer () =
  for i = 1 to 5 do
    Mutex.lock mutex;
    Queue.push ("task " ^ string_of_int i) queue;

    (* Сообщаем, что очередь не пустая *)
    Condition.signal cond;

    Mutex.unlock mutex;
    Thread.delay 1.0  (* имитируем работу *)
  done

let () =
  let c = Thread.create consumer () in
  let p = Thread.create producer () in
  Thread.join c;
  Thread.join p
