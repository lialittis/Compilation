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
    mutable fby_next1: float;
  }

let integr'_1_init () = {
    fby_next1 = 0.;
  }

let integr'_1_step mem' (t) = 
  let (aux'1) = t in
  let (aux'2) = (mem'.fby_next1) in
  let (y) = aux'2 in
  mem'.fby_next1 <- aux'1;
  (y)


type test'_2_mem = {
    mutable fby_next2: float;
    integr_mem1: integr'_1_mem;
  }

let test'_2_init () = {
    fby_next2 = 0.;
    integr_mem1 = integr'_1_init ();
  }

let test'_2_step mem' (i) = 
  let (aux'4) = (mem'.fby_next2) in
  let (aux'6) = (
    (print_string "coucou\n");
    flush_all()) in
  let (x0) = 1. in
  let (aux'3) = x0 in
  let (o) = aux'6 in
  let (x1) = aux'4 in
  let (aux'5) = (integr'_1_step mem'.integr_mem1 (x1)) in
  let (x2) = aux'5 in
  mem'.fby_next2 <- aux'3;
  (o)


let wait () = try Thread.delay 0.01 with _ -> ()

let run f_init f_step =
  let mem = f_init () in
  while true do
    Graphics.clear_graph ();
    f_step mem ();
    Graphics.synchronize();
    wait()
  done

let _ = run test'_2_init test'_2_step