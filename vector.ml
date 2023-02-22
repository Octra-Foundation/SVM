let c_max_value = 65535
let r_l_value = 512
let r_s_value = 82
let n_values = [|2048; 65536; 65536; 131072|]
let s1_state = ref 0
let s2_state = ref 0
let count_value = ref 0
let w_weight = ref 0

let octra_vector y =
  s1_state := (!s1_state lsl 1) lor y;
  let k_value = (!count_value mod 2) + 2 * y in
  s2_state := (!s2_state lsl 1) * (k_value mod 2) + (!s2_state + (2 * k_value - 1)) * (k_value lsr 1);
  count_value := !count_value + 1;

  let x0_value = (!s1_state land 0xff) lor ((!count_value lsl 8) land (n_values.(0) - 1)) in
  let x1_value = (x0_value + (!s2_state land (n_values.(1) - 1))) land (n_values.(1) - 1) in 
  let x2_value = (x1_value + (!s2_state land (n_values.(2) - 1))) land (n_values.(2) - 1) in
  let x3_value = (x2_value + (!s2_state land (n_values.(3) - 1))) land (n_values.(3) - 1) in

  let error = (y lsl 16) - (!w_weight lsr 16) in
  w_weight := !w_weight + (error * (r_s_value + 2 * r_l_value * (x0_value + x1_value + x2_value + x3_value)));
  if !w_weight > c_max_value || !w_weight < -c_max_value then
    w_weight := (!w_weight asr 1) + (if !w_weight > 0 then 1 else -1);

  let exponent = float_of_int (!w_weight lsr 8) in
  let pr_value = 1.0 /. (1.0 +. exp (-1.0 *. exponent)) in
  pr_value ;;
