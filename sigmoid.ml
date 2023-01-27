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



(* Test *)

let s = Sigmoid.create 10 in
let p = 0.8 in
let result = Sigmoid.logit s p in
print_endline (string_of_float result)
