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


type integr'_1_mem = {
    mutable aux'2_next1: float;
  }

let integr'_1_init () = {
    aux'2_next1 = 0.;
  }

let integr'_1_step mem' (t, dx) = 
  let (aux'2) = mem'.aux'2_next1 in
  let (x) = aux'2 in
  let (aux'1) = ((t  *.  dx)  +.  x) in
  mem'.aux'2_next1 <- aux'1;
  (x)


type deriv'_2_mem = {
    mutable aux'4_next2: float;
  }

let deriv'_2_init () = {
    aux'4_next2 = 0.;
  }

let deriv'_2_step mem' (t, x) = 
  let (aux'3) = x in
  let (aux'4) = mem'.aux'4_next2 in
  let (dx) = ((x  -.  aux'4)  /.  t) in
  mem'.aux'4_next2 <- aux'3;
  (dx)


type integr'_3_mem = {
    integr_mem1: integr'_1_mem;
  }

let integr'_3_init () = {
    integr_mem1 = integr'_1_init ();
  }

let integr'_3_step mem' (dx) = 
  let (aux'5) = (integr'_1_step mem'.integr_mem1 (0.05, dx)) in
  let (x) = aux'5 in
  (x)


type deriv'_4_mem = {
    deriv_mem2: deriv'_2_mem;
  }

let deriv'_4_init () = {
    deriv_mem2 = deriv'_2_init ();
  }

let deriv'_4_step mem' (x) = 
  let (aux'6) = (deriv'_2_step mem'.deriv_mem2 (0.05, x)) in
  let (dx) = aux'6 in
  (dx)


type equation'_5_mem = {
    mutable aux'12_next3: float;
    integr_mem4: integr'_3_mem;
    integr_mem3: integr'_3_mem;
  }

let equation'_5_init () = {
    aux'12_next3 = 0.;
    integr_mem4 = integr'_3_init ();
    integr_mem3 = integr'_3_init ();
  }

let equation'_5_step mem' (d2x0, d2y0) = 
  let (aux'12) = mem'.aux'12_next3 in
  let (thetap) = aux'12 in
  let (aux'7) = (sin (thetap)) in
  let (aux'8) = (cos (thetap)) in
  let (aux'9) = (integr'_3_step mem'.integr_mem4 (((aux'7  *.  (d2y0  +.  9.81))  -.  (aux'8  *.  d2x0)))) in
  let (aux'10) = (integr'_3_step mem'.integr_mem3 (((aux'9  /.  10.)  -.  (1.2  *.  thetap)))) in
  let (theta) = aux'10 in
  let (aux'11) = theta in
  mem'.aux'12_next3 <- aux'11;
  (theta)


type position'_6_mem = {
    deriv_mem9: deriv'_4_mem;
    deriv_mem8: deriv'_4_mem;
    deriv_mem7: deriv'_4_mem;
    deriv_mem6: deriv'_4_mem;
    equation_mem5: equation'_5_mem;
  }

let position'_6_init () = {
    deriv_mem9 = deriv'_4_init ();
    deriv_mem8 = deriv'_4_init ();
    deriv_mem7 = deriv'_4_init ();
    deriv_mem6 = deriv'_4_init ();
    equation_mem5 = equation'_5_init ();
  }

let position'_6_step mem' (x0, y0) = 
  let (aux'13) = (deriv'_4_step mem'.deriv_mem9 (x0)) in
  let (aux'15) = (deriv'_4_step mem'.deriv_mem8 (y0)) in
  let (aux'14) = (deriv'_4_step mem'.deriv_mem7 (aux'13)) in
  let (aux'16) = (deriv'_4_step mem'.deriv_mem6 (aux'15)) in
  let (d2x0) = aux'14 in
  let (d2y0) = aux'16 in
  let (aux'17) = (equation'_5_step mem'.equation_mem5 (d2x0, d2y0)) in
  let (theta) = aux'17 in
  let (aux'18) = (sin (theta)) in
  let (aux'19) = (cos (theta)) in
  let (x) = (x0  +.  (10.  *.  aux'18)) in
  let (y) = (y0  +.  (10.  *.  aux'19)) in
  (x, y)


type get_cursor'_7_mem = unit

let get_cursor'_7_init () = ()

let get_cursor'_7_step mem' () = 
  let (aux'20, aux'21) = (get_mouse (())) in
  let (mx, my) = (aux'20, aux'21) in
  let (aux'22) = (float_of_int (mx)) in
  let (aux'23) = (float_of_int (my)) in
  let (x, y) = ((aux'22  /.  10.), (aux'23  /.  10.)) in
  (x, y)


type draw_pendulum'_8_mem = unit

let draw_pendulum'_8_init () = ()

let draw_pendulum'_8_step mem' (x0, y0, x, y) = 
  let (aux'24) = (int_of_float ((10.  *.  x0))) in
  let (aux'25) = (int_of_float ((10.  *.  x))) in
  let (aux'26) = (int_of_float ((10.  *.  y0))) in
  let (aux'27) = (int_of_float ((10.  *.  y))) in
  let (ix) = aux'25 in
  let (ix0) = aux'24 in
  let (iy) = aux'27 in
  let (iy0) = aux'26 in
  let (aux'28) = (draw_line (ix0, iy0, ix, iy)) in
  let (aux'29) = (draw_circle (ix, iy, 5)) in
  let (o) = 
    (if false then
       aux'28
     else
       aux'29) in
  (o)


type play'_9_mem = {
    get_cursor_mem12: get_cursor'_7_mem;
    position_mem11: position'_6_mem;
    draw_pendulum_mem10: draw_pendulum'_8_mem;
  }

let play'_9_init () = {
    get_cursor_mem12 = get_cursor'_7_init ();
    position_mem11 = position'_6_init ();
    draw_pendulum_mem10 = draw_pendulum'_8_init ();
  }

let play'_9_step mem' (u) = 
  let (aux'30, aux'31) = (get_cursor'_7_step mem'.get_cursor_mem12 ()) in
  let (x0, y0) = (aux'30, aux'31) in
  let (aux'32, aux'33) = (position'_6_step mem'.position_mem11 (x0, y0)) in
  let (x, y) = (aux'32, aux'33) in
  let (aux'34) = (draw_pendulum'_8_step mem'.draw_pendulum_mem10 (x0, y0, x, y)) in
  let (u2) = aux'34 in
  (u2)


let wait () = try Thread.delay 0.01 with _ -> ()

let run f_init f_step =
  let mem = f_init () in
  while true do
    Graphics.clear_graph ();
    f_step mem ();
    Graphics.synchronize();
    wait()
  done

let _ = run play'_9_init play'_9_step