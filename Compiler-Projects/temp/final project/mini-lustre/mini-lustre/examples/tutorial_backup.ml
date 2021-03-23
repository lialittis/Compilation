let print_bool b = 
  if b then print_string "true" 
  else print_string "false";;

let read = read_line
let random_int = Random.int
let random_float = Random.float
let draw_point (x,y) = Graphics.plot x y
let draw_line (x0,y0,x,y) =
  Graphics.moveto x0 y0;
  Graphics.lineto x y;;

let draw_circle (x,y,r) = Graphics.draw_circle x y r;;

let draw_rect (x,y,w,h) = Graphics.draw_rect x y w h;;

let fill_rect (x,y,w,h) = Graphics.fill_rect x y w h;;

let get_mouse = Graphics.mouse_pos
let _x' = Graphics.open_graph "";;

let _x' = Graphics.auto_synchronize false;;


type dt'_1_mem = unit

let dt'_1_init () = ()

let dt'_1_step mem' () = 
  let (dt) = 0.001 in
  (dt)


type g'_2_mem = unit

let g'_2_init () = ()

let g'_2_step mem' () = 
  let (g) = 0.001 in
  (g)


type init'_3_mem = {
    mutable aux'2_next1: bool;
  }

let init'_3_init () = {
    aux'2_next1 = true;
  }

let init'_3_step mem' () = 
  let (aux'1) = false in
  let (aux'2) = mem'.aux'2_next1 in
  let (o) = aux'2 in
  mem'.aux'2_next1 <- aux'1;
  (o)


type average'_4_mem = unit

let average'_4_init () = ()

let average'_4_step mem' (x, y) = 
  let (o) = ((x  +  y)  /  2) in
  (o)


type xor'_5_mem = unit

let xor'_5_init () = ()

let xor'_5_step mem' (a, b) = 
  let (o) = ((a  &&  (not b))  ||  ((not a)  &&  b)) in
  (o)


type full_add'_6_mem = {
    xor_mem2: xor'_5_mem;
    xor_mem1: xor'_5_mem;
  }

let full_add'_6_init () = {
    xor_mem2 = xor'_5_init ();
    xor_mem1 = xor'_5_init ();
  }

let full_add'_6_step mem' (a, b, c) = 
  let (aux'3) = (xor'_5_step mem'.xor_mem2 (a, b)) in
  let (co) = (((a  &&  b)  ||  (b  &&  c))  ||  (a  &&  c)) in
  let (aux'4) = (xor'_5_step mem'.xor_mem1 (aux'3, c)) in
  let (s) = aux'4 in
  (s, co)


type half_add'_7_mem = {
    xor_mem3: xor'_5_mem;
  }

let half_add'_7_init () = {
    xor_mem3 = xor'_5_init ();
  }

let half_add'_7_step mem' (a, b) = 
  let (aux'5) = (xor'_5_step mem'.xor_mem3 (a, b)) in
  let (co) = (a  &&  b) in
  let (s) = aux'5 in
  (s, co)


type full_add'_8_mem = {
    half_add_mem5: half_add'_7_mem;
    half_add_mem4: half_add'_7_mem;
  }

let full_add'_8_init () = {
    half_add_mem5 = half_add'_7_init ();
    half_add_mem4 = half_add'_7_init ();
  }

let full_add'_8_step mem' (a, b, c) = 
  let (aux'6, aux'7) = (half_add'_7_step mem'.half_add_mem5 (a, b)) in
  let (s1, c1) = (aux'6, aux'7) in
  let (aux'8, aux'9) = (half_add'_7_step mem'.half_add_mem4 (c, s1)) in
  let (s, c2) = (aux'8, aux'9) in
  let (co) = (c1  ||  c2) in
  (s, co)


type nat'_9_mem = {
    mutable aux'11_next2: int;
  }

let nat'_9_init () = {
    aux'11_next2 = 0;
  }

let nat'_9_step mem' (m) = 
  let (aux'11) = mem'.aux'11_next2 in
  let (o) = aux'11 in
  let (aux'10) = (o  +  1) in
  mem'.aux'11_next2 <- aux'10;
  (o)


type edge'_10_mem = {
    mutable aux'13_next3: bool;
  }

let edge'_10_init () = {
    aux'13_next3 = false;
  }

let edge'_10_step mem' (c) = 
  let (aux'12) = c in
  let (aux'13) = mem'.aux'13_next3 in
  let (o) = (c  &&  (not aux'13)) in
  mem'.aux'13_next3 <- aux'12;
  (o)


type integr'_11_mem = {
    mutable aux'15_next4: float;
    dt_mem6: dt'_1_mem;
  }

let integr'_11_init () = {
    aux'15_next4 = 0.;
    dt_mem6 = dt'_1_init ();
  }

let integr'_11_step mem' (dx) = 
  let (aux'15) = mem'.aux'15_next4 in
  let (aux'16) = (dt'_1_step mem'.dt_mem6 ()) in
  let (x) = (aux'15  +.  (dx  *.  aux'16)) in
  let (aux'14) = x in
  mem'.aux'15_next4 <- aux'14;
  (x)


type double_integr'_12_mem = {
    integr_mem8: integr'_11_mem;
    integr_mem7: integr'_11_mem;
  }

let double_integr'_12_init () = {
    integr_mem8 = integr'_11_init ();
    integr_mem7 = integr'_11_init ();
  }

let double_integr'_12_step mem' (d2x) = 
  let (aux'18) = (integr'_11_step mem'.integr_mem8 (d2x)) in
  let (dx) = aux'18 in
  let (aux'17) = (integr'_11_step mem'.integr_mem7 (dx)) in
  let (x) = aux'17 in
  (x)


type min_max'_13_mem = {
    mutable aux'20_next7: bool;
    mutable aux'23_next5: int;
    mutable aux'24_next6: int;
  }

let min_max'_13_init () = {
    aux'20_next7 = true;
    aux'23_next5 = 0;
    aux'24_next6 = 0;
  }

let min_max'_13_step mem' (x) = 
  let (aux'19) = false in
  let (aux'20) = mem'.aux'20_next7 in
  let (aux'23, aux'24) = (mem'.aux'23_next5, mem'.aux'24_next6) in
  let (first) = aux'20 in
  let (pmin, pmax) = 
    (if first then
       (x, x)
     else
       (aux'23, aux'24)) in
  let (min, max) = 
    (if (x  <  pmin) then
       (x, pmax)
     else
       
    (if (x  >  pmax) then
       (pmin, x)
     else
       (pmin, pmax))) in
  let (aux'21, aux'22) = (min, max) in
  mem'.aux'20_next7 <- aux'19;
  mem'.aux'23_next5 <- aux'21;
  mem'.aux'24_next6 <- aux'22;
  (min, max)


