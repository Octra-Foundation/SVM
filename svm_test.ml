open Svm
open Lacaml.D

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
