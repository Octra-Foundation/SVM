open Svm
open Lacaml.D
open Printf

let data = [((1., 0.), 1); ((0., 1.), -1); ((0., 0.), -1); ((1., 1.), 1)]

let x, y = List.split (List.map fst data)
let x = Mat.of_rows x

let params = { Svm.svm_type = Svm.C_SVC;
               kernel_type = RBF;
               C = 1.;
               gamma = 0.5 }

let svm = Svm.train data params

let classify point = Svm.predict svm point
let classify_matrix m = Mat.init (Mat.dim2 m) classify m

let result = classify_matrix x

let f n =
  let sqrt_term = sqrt(16.0 ** (float_of_int n) +. 1.0) in
  let inner_term = 2.0 *. 4.0 ** (float_of_int n) +. 2.0 *. sqrt_term in
  1.0 +. sqrt(inner_term) /. 2.0 ** (2.0 *. (float_of_int n) +. 2.0)
;;

module Sigmoid = struct
  type t = { logit_size : int; logit_table : float array }

  let create logit_size =
    let logit_table = Array.init logit_size (fun i -> log ((float (i + 1)) /. (float (logit_size + 1))) -. log (1. -. (float (i + 1)) /. (float (logit_size + 1)))) in
    { logit_size; logit_table }

  let logit { logit_size; logit_table } p =
    let index = int_of_float (p *. float logit_size) in
    if index >= logit_size then logit_table.(logit_size - 1)
    else if index < 0 then logit_table.(0)
    else logit_table.(index)

  let logistic p = 1. /. (1. +. exp (-.p))
end

let init_state () =
  let m1 = Array.make_matrix 100 100 0. in
  let m2 = Array.make_matrix 100 100 0. in
  let m_res = Array.make_matrix 100 100 0. in
  for i = 0 to 99 do
    for j = 0 to 99 do
      m1.(i).(j) <- f (i * 100 + j);
      m2.(i).(j) <- Sigmoid.logistic m1.(i).(j);
    done
  done;
  for i = 0 to 99 do
    for j = 0 to 99 do
      for k = 0 to 99 do
        m_res.(i).(j) <- m_res.(i).(j) +. m1.(i).(k) *. m2.(k).(j);
      done
    done
  done;
  m_res
;;

let print_float_matrix mat =
  Array.iter (fun row -> Array.iter (Printf.printf "%f ") row; print_endline "") mat
;;

let result = init_state () in
print_float_matrix result 
