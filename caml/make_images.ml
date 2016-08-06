open Batteries
open Num
open IO
open Graphics
open List
open Basics
open Myparsing


let () =
  open_graph "";
  set_window_title "youhou";
  resize_window width width;
  for i = 1 to 2730 do
    clear_graph ();
    try
      draw_problem (parse_problem ("../pb/" ^ (string_of_int i) ^ ".pb"));
      let img = get_image 0 0 width width in
      Images.sauver_image (dump_image img) ("../pb/" ^ (string_of_int i) ^ ".png");
    with
    | _ -> ()
  done;
  close_graph ()


