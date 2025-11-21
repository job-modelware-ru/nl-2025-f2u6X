(*
  Примеры к презентации 06-ml / 05
  Язык ML: обобщённые типы, функциональное и ОО-программирование
*)

(*******************************)
(* 01. Обобщённые типы         *)
(*******************************)

(* Тождественная функция *)
fun id x = x;

(* Обобщённая функция длины списка *)
fun listLength xs =
  let
    fun aux (acc, [])      = acc
      | aux (acc, _ :: tl) = aux (acc + 1, tl)
  in
    aux (0, xs)
  end;

(* Демонстрация работы id и listLength *)
val _ =
  let
    val ints    = [1, 2, 3]
    val strings = ["a", "b", "c", "d"]
  in
    print ("len [1,2,3] = " ^
           Int.toString (listLength ints) ^ "\n");
    print ("len [\"a\",\"b\",\"c\",\"d\"] = " ^
           Int.toString (listLength strings) ^ "\n");
    print ("id 42 = " ^ Int.toString (id 42) ^ "\n");
    print ("id \"hello\" = " ^ id "hello" ^ "\n")
  end;


(*******************************)
(* 02. Функциональный стиль    *)
(*******************************)

(* Своя rev — чтобы не зависеть от List.rev *)
fun rev xs =
  let
    fun loop ([], acc)      = acc
      | loop (y :: ys, acc) = loop (ys, y :: acc)
  in
    loop (xs, [])
  end;

(* Обобщённый map *)
fun map f xs =
  let
    fun loop ([], acc)      = rev acc
      | loop (y :: ys, acc) = loop (ys, f y :: acc)
  in
    loop (xs, [])
  end;

(* Обобщённый filter *)
fun filter p xs =
  let
    fun loop ([], acc) = rev acc
      | loop (y :: ys, acc) =
          if p y
          then loop (ys, y :: acc)
          else loop (ys, acc)
  in
    loop (xs, [])
  end;

(* Красивый вывод списка int-ов *)
fun showIntList xs =
  let
    fun loop [] acc        = acc ^ "]"
      | loop [x] acc       = acc ^ Int.toString x ^ "]"
      | loop (x :: xs) acc = loop xs (acc ^ Int.toString x ^ "; ")
  in
    loop xs "["
  end;

(* Демонстрация работы map/filter/showIntList *)
val _ =
  let
    val nums    = [1, 2, 3, 4, 5]
    val squares = map (fn x => x * x) nums
    val evens   = filter (fn x => x mod 2 = 0) nums
  in
    print ("nums    = " ^ showIntList nums ^ "\n");
    print ("squares = " ^ showIntList squares ^ "\n");
    print ("evens   = " ^ showIntList evens ^ "\n")
  end;


(*******************************)
(* 03. Ввод-вывод (TextIO)     *)
(*******************************)

(* Подсчёт строк в текстовом файле с помощью TextIO *)
fun countLines ins =
  let
    fun loop n =
      case TextIO.inputLine ins of
           SOME _ => loop (n + 1)
         | NONE   => n
  in
    loop 0
  end;

(* Демонстрация: запрос имени файла, подсчёт строк и вывод результата *)
val _ =
  let
    val () = print "Введите имя файла и нажмите Enter:\n"
  in
    case TextIO.inputLine TextIO.stdIn of
         SOME name =>
           let
             val len      = String.size name
             (* Убираем символ перевода строки в конце *)
             val filename =
               if len > 0
               then String.substring (name, 0, len - 1)
               else name
             val ins    = TextIO.openIn filename
             val lines  = countLines ins
             val ()     = TextIO.closeIn ins
           in
             print ("В файле \"" ^ filename ^ "\" "
                    ^ Int.toString lines ^ " строк(и)\n")
           end
       | NONE =>
           print "Не удалось прочитать имя файла\n"
  end;


(*******************************)
(* 04. Конкурентность (модель) *)
(*******************************)

(* Упрощённая модель CML-подобных каналов *)

structure CML =
struct
  datatype 'a chan = Chan of 'a list ref

  fun channel () = Chan (ref [])

  (* Отправка: добавить сообщение в конец списка *)
  fun send (Chan r, x) =
    r := (!r) @ [x]

  (* Приём: взять первое сообщение, если оно есть *)
  fun recv (Chan r) =
    case !r of
         []      => "нет сообщений\n"
       | x :: xs => (r := xs; x)
end;

structure Example =
struct
  open CML

  (* "Рабочий": печатает шаги и шлёт сообщения в канал *)
  fun worker name ch =
    let
      fun loop 0 = ()
        | loop n =
            ( print (name ^ ": шаг " ^
                     Int.toString n ^ "\n");
              send (ch, name ^ " готов\n");
              loop (n - 1) )
    in
      loop 3
    end;

  (* "Логгер": несколько раз читает сообщения и печатает их *)
  fun logger ch =
    let
      fun loop 0 = ()
        | loop n =
            let
              val msg = recv ch
            in
              print ("Лог: " ^ msg);
              loop (n - 1)
            end
    in
      loop 6
    end;

  (* Демонстрация: два "воркера" и один "логгер" *)
  val _ =
    let
      val ch = channel ()
      val _  = worker "Поток 1" ch
      val _  = worker "Поток 2" ch
      val _  = logger ch
    in
      ()
    end
end;


(*******************************)
(* 05. «ООП» через записи      *)
(*******************************)

type shape =
  { name : string
  , area : unit -> int
  };

fun makeCircle r =
  { name = "Круг"
  , area = (fn () => r * r)   (* упрощённая формула площади *)
  };

fun makeRectangle (w, h) =
  { name = "Прямоугольник"
  , area = (fn () => w * h)
  };

fun describe (s : shape) =
  s.name ^ " с площадью " ^
  Int.toString (s.area ());

(* Демонстрация: список фигур и их описания *)
val _ =
  let
    val shapes : shape list =
      [ makeCircle 3
      , makeRectangle (2, 3)
      ]
  in
    List.app (fn s =>
      print (describe s ^ "\n")) shapes
  end;

