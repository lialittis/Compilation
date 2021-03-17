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


type n'_1_mem = {
    mutable x_next1: float;
  }

let n'_1_init () = {
    x_next1 = 0.;
  }

let n'_1_step mem' (i) = 
  let (aux'1) = (
    (print_string "coucou\n");
    flush_all()) in
  let (x0) = 1. in
  let (o) = aux'1 in
  let (x) = x0 in
  mem'.x_next1 <- x0;
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

let _ = run n'_1_init n'_1_step