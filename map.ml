class state = object
  method init_probability (x: int) : float = 
    float_of_int x *. 2.0
  method next (x: int) (y: int) : int = 
    x + y
end ;;

class run_map = object (self : 'a)
  inherit state
  val table_ : int array = Array.make 512 0
  method init () =
    for i = 0 to 511 do
      let state = i / 2 in
      if i mod 2 = 0 then
        table_.(i) <- if state < 127 then state + 1 else if state >= 128 then 0 else state
      else
        table_.(i) <- if state < 128 then 128 else if state < 255 then state + 1 else state
    done
  method init_probability state =
    if state < 128 then (128.0 -. float_of_int state) /. 256.0 else float_of_int state /. 256.0
  method next state bit = table_.(state * 2 + bit)
end;;
