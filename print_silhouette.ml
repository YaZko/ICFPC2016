(* OCaml toplevel to run: ocaml graphics.cma *)
open Batteries
open BatNum
open BatIO
open Graphics
       
type point = num * num
type segment = point * point
			 
type polygon = point list

type silhouette = polygon list
type skeleton = segment list
			  
let parse_num (s : string) : num =
  try
    let s1, s2 = BatString.split s "/" in
    (of_string s1) / (of_string s2)
  with
  | Not_found -> of_string s
			    
let parse_point (s : string) : point =
  let s1, s2 = BatString.split s "," in
  parse_num s1, parse_num s2

let parse_segment (s : string) : segment =
  let s1, s2 = BatString.split s " " in
  parse_point s1, parse_point s2
  

let parse_problem (filename : string) : silhouette * skeleton =
  let f = open_in filename in
  (* parse silhouette *)
  let nb_poly = int_of_string (read_line f) in
  let silh = ref [] in
  for p = 1 to nb_poly do
    let poly = ref [] in
    let nb_vertices = int_of_string (read_line f) in
    for i = 1 to nb_vertices do
      let v = parse_point (read_line f) in
      poly := !poly @ [v]
    done;
    silh := !silh @ [!poly]
  done;
  (* parse skeleton *)
  let nb_seg = int_of_string (read_line f) in
  let skel = ref [] in
  for p = 1 to nb_seg do
    skel := !skel @ [parse_segment (read_line f)];
  done;
  !silh, !skel

  
	    

let is_positive (p : polygon) : bool =
  true


let width = 950
	      
let print_polygon (sat : num * num -> int * int) (p : polygon) : unit =
  if is_positive p then
    set_color red
  else set_color white;
  let p' = Array.map sat (BatArray.of_list p) in
  fill_poly p'

let print_segment (sat : num * num -> int * int) (s1, s2 : segment) : unit =
  set_color black;
  set_line_width 3;
  let x,y = sat s1 in
  let x', y' = sat s2 in
  draw_segments [| x, y, x', y' |]
	      
let print_problem (s, sk : silhouette * skeleton) : unit =
  let points = List.concat s in
  let xs = List.map fst points in
  let ys = List.map snd points in
  let minx, maxx = BatList.min_max ~cmp:compare_num xs in
  let miny, maxy = BatList.min_max ~cmp:compare_num ys in
  let minx = (min_num zero minx) - (of_float 0.01) in
  let maxx = (max_num one maxx) + (of_float 0.01) in
  let miny = (min_num zero miny) - (of_float 0.01) in
  let maxy = (max_num one maxy) + (of_float 0.01) in
  let maxmax = max_num (maxx - minx) (maxy - miny) in
  let scale_and_translate (x,y : num * num) : int * int =
    to_int (approx (((x - minx) * of_int width) / maxmax)),
    to_int (approx (((y - miny) * of_int width) / maxmax))
  in
  (* draw unit square *)
  set_color blue;
  let sq = [| zero, zero; one, zero; one, one; zero, one |] in
  fill_poly (Array.map scale_and_translate sq);
  (* draw the polygons *)
  List.iter (print_polygon scale_and_translate) s;
  (* draw the skeleton *)
  List.iter (print_segment scale_and_translate) sk;
  (* press a key to exit *)
  (* let _ = read_key () in *)
  (* close_graph () *)
	    
open Images;;

let () =
  open_graph "";
  set_window_title "youhou";
  resize_window width width;
  for i = 1 to 100 do
    clear_graph ();
    print_problem (parse_problem ("pb/" ^ (string_of_int i) ^ ".pb"));
    let img = get_image 0 0 width width in
    sauver_image (dump_image img) ("pb/" ^ (string_of_int i) ^ ".png");
  done;
  close_graph ()
