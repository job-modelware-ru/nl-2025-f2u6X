class rectangle width height =
  object
    inherit shape "Rectangle"

    method area = float width *. float height
    method perimeter = 2. *. (float width +. float height)
  end

let shapes : shape list = [
  (new circle 5.0 :> shape);
  (new rectangle 4 3 :> shape)
]

let print_all_shapes shapes =
  List.iter (fun shape -> shape#print_info) shapes