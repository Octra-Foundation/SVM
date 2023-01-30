let iteration_count = 1;;
let constant_k = 1.0;; (* the constant k for the Spiral Computation Algorithm *)

let atan_lookup_table = Array.init iteration_count (fun i -> atan (2.0 ** (float (-i))));;

let spiral_computation_algorithm input_angle =
  let x_coordinate = ref constant_k in
  let y_coordinate = ref 0. in
  let angle = ref input_angle in
  for i = 0 to iteration_count - 1 do
    let direction = if !angle >= 0. then 1. else -1. in
    let new_x = !x_coordinate -. direction *. !y_coordinate *. atan_lookup_table.(i) in
    let new_y = !y_coordinate +. direction *. !x_coordinate *. atan_lookup_table.(i) in
    let new_angle = !angle -. direction *. atan_lookup_table.(i) in
    x_coordinate := new_x;
    y_coordinate := new_y;
    angle := new_angle;
  done;
  atan2 !y_coordinate !x_coordinate;;
