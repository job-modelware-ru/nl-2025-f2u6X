(* 1. Переменные — объявление *)
(* объявление неизменяемых переменных *)
val x = 10;
val s = "hello";
val pair = (x, s);

(* После этого нельзя написать "x := 20" — это ошибка *)
(* Попытка "val x = 20;" в той же области видимости создаст shadowing, не переопределение *)

(* ref — контейнер с изменяемым содержимым *)
val r = ref 0;        (* r : int ref *)
r := !r + 1;          (* обновление значения внутри *)
val after = !r;       (* чтение: !r - разыменование *)
(* r сам фиксирован (val r = ...), но содержимое изменяемо *)

val arr = Array.array (5, 0);  (* массив из 5 нулей *)
Array.update (arr, 2, 42);
val v = Array.sub (arr, 2);    (* v = 42 *)

val x = 100;



(* 2. Области видимости (scope) *)

val result =
  let
    val x = 5        (* локальный x скрывает глобальный x *)
    val y = x + 10
  in
    x * y
  end;               (* result = 5 * 15 = 75 *)

(* внешний x остаётся 100 *)

local
  val secret = "pwd123"
in
  fun get_stub () = "secret hidden"
end;

(* secret недоступна вне блока local *)

fun processList lst =
  case lst of
      [] => "empty"
    | x::xs => let val head = x in head::xs end;  (* head доступен только внутри *)


(* 3. Владение и передача владения *)

(* Модуль, который владеет ресурсом *)
structure Buffer :> sig
  type t
  val create : unit -> t
  val push : t * int -> unit
  val toList : t -> int list
end = struct
  type t = { data : int list ref }  (* внутренний мутабельный тип *)
  fun create () = { data = ref [] }
  fun push ({data}, v) = data := v :: !data
  fun toList {data} = rev (!data)
end;

(* Другие части программы получают только абстракцию t, не знают внутренностей *)


(* представим ресурс, который нужно "передать" и сделать недоступным у отправителя *)
datatype resource = R of string

(* функция move: берет resource option, возвращает новый option и высланный ресурс *)
fun move (NONE) = (NONE, NONE)
  | move (SOME r) = (NONE, SOME r);

(* использование *)
val senderRef = ref (SOME (R "file_handle_1"));

(* отправка *)
val ((), receiverOpt) =
  case !senderRef of
      NONE => (print "nothing to send\n"; ((), NONE))
    | SOME r => (senderRef := NONE; ((), SOME r));

(* теперь senderRef содержит NONE — имитация передачи владения *)

(* 4. Функции — объявление и входные данные *)

fun add x y = x + y;   (* тип: int -> int -> int *)
val add5 = add 5;      (* частичное применение: add5 : int -> int *)
val r = add5 7;        (* r = 12 *)

fun inc n = n + 1;
val a = 10;
val b = inc a;   (* a остаётся 10 *)

fun appendRef (rRef: int ref, v: int) = rRef := !rRef + v;

val myRef = ref 3;
appendRef (myRef, 4);   (* myRef теперь содержит 7 *)

(* 5. Выходные данные функции (return) *)

fun div_mod (a, b) =
  if b = 0 then raise Fail "division by zero"
  else (a div b, a mod b);

val (q, r) = div_mod (17, 4);  (* q=4, r=1 *)

fun safe_div (a, b) =
  if b = 0 then NONE
  else SOME (a div b);

case safe_div (10, 0) of
    NONE => print "cannot divide\n"
  | SOME q => print ("quotient = " ^ Int.toString q ^ "\n")

(* 6. Рекурсия *)

fun factorial 0 = 1
  | factorial n = n * factorial (n - 1);

(* маленький n — fine *)

fun fact_tail n =
  let
    fun loop (0, acc) = acc
      | loop (k, acc) = loop (k - 1, k * acc)
  in
    loop (n, 1)
  end;

fun map f lst =
  let
    fun loop ([], acc) = rev acc
      | loop (x::xs, acc) = loop (xs, f x :: acc)
  in
    loop (lst, [])
  end;
