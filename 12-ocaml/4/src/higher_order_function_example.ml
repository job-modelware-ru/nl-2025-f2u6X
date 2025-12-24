type user = { name: string; age: int; active: bool; }

let users = [
  { name = "Nikita"; age = 22; active = false };
  { name = "Artyom"; age = 22; active = true };
  { name = "Artur"; age = 17; active = true };
]

let process_users user_list filter_func map_func = 
  List.filter filter_func user_list
  |> List.map map_func


let result = process_users
  users
  (fun usr -> usr.active)
  (fun usr -> {usr with name = usr.name ^ " is cool user"})
