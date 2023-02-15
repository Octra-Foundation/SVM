let sigmoid (d: int) : int =
  let open Float in
  if d < -2047 then 1
  else if d > 2047 then 4095
  else
    let p = 1.0 / (1.0 + exp(-d / 256.0)) * 4096.0 in
    let pi = p |> round |> int_of_float in
    if pi > 4095 then 4095
    else if pi < 1 then 1
    else pi

let make_sigmoid_table () : int array =
  let t = Array.make 4095 0 in
  for i = -2047 to 2047 do
    t.(i + 2047) <- sigmoid i
  done;
  t

let sigmoid_table = make_sigmoid_table ()

let sigmoid_lookup (d: int) : int =
  if d < -2047 then 1
  else if d > 2047 then 4095
  else sigmoid_table.(d + 2047)
