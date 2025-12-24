type user = { name: string; age: int }

let user1 = { name = "Alice"; age = 25 }
(* user1 теперь неизменяем - нельзя сделать user1.age = 26 *)

(* Вместо этого создаем нового пользователя: *)
let user2 = { user1 with age = 26 }
(* user1 остался {name = "Alice"; age = 25} *)
(* user2 стал {name = "Alice"; age = 26} *)
