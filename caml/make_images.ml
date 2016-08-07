open Batteries
open Num
open IO
open Graphics
open List
open Basics
open Myparsing

let dirpath = "../pb/"
		
let () =
  open_graph "";
  set_window_title "youhou";
  resize_window width width;
  for i = 1 to 200 do
    clear_graph ();
    try
      draw_problem (parse_problem (dirpath ^ (string_of_int i) ^ ".pb"));
      let img = get_image 0 0 width width in
      Images.sauver_image (dump_image img) (dirpath ^ (string_of_int i) ^ ".png");
      (* clear_graph (); *)
      (* draw_problem (parse_problem ("../pb/notconvex/" ^ (string_of_int i) ^ "_hull.pb")); *)
      (* let img = get_image 0 0 width width in *)
      (* Images.sauver_image (dump_image img) ("../pb/notconvex/" ^ (string_of_int i) ^ "_hull.png"); *)
    with
    | _ -> ()
  done;
  close_graph ()


