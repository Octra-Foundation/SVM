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

let () =
  for i = 1 to 100000 do
    printf "%f\n" (f i)
  done
