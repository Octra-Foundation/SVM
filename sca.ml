open Cryptokit

let iteration_count = 1;;
let constant_k = 1.0;; (* the constant k for the Spiral Computation Algorithm *)
type pair = float * float
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

let rec sort_data_points lst =
  match lst with
  | [] -> []
  | hd::tl ->
    let left, right = List.partition (fun x -> fst x <= fst hd) tl in
    sort_data_points left @ [hd] @ sort_data_points right

let adaptive_polf x y =
  let data = List.combine x y in
  let sorted_data = sort_data_points data in
  let rec get_slopes data_points acc =
    match data_points with
    | [] -> acc
    | hd::[] -> acc
    | (x1, y1)::(x2, y2)::tl ->
      let slope = (y2 -. y1) /. (x2 -. x1) in
      get_slopes ((x2, y2)::tl) (slope::acc)
  in
  let slopes = List.rev (get_slopes sorted_data []) in
  let rec find_piecewise_linear_function x = function
    | [] -> raise Not_found
    | (x1, y1)::[] -> y1
    | (x1, y1)::(x2, y2)::tl when x1 <= x && x <= x2 ->
      y1 +. (x -. x1) *. (List.hd slopes)
    | (x1, y1)::(x2, y2)::tl -> find_piecewise_linear_function x ((x2, y2)::tl)
  in
  find_piecewise_linear_function
  
let validate_node_parameter (octra_node_VT: string) =
  let id = ref 0 in
  let sha256 = Cryptokit.Hash.sha256 () in
  let hash_code = sha256#add_string octra_node_VT |> sha256#result in
  let true_score = ref [] in
  while !id < 20 do
    let value = hash_code in
    let VT = adaptive_polf octra_node_VT in
    true_score := (!id, value, VT) :: !true_score;
    id := !id + 1;
  done;
  !true_score;;
