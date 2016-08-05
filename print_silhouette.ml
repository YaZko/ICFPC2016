(* OCaml toplevel to run: ocaml graphics.cma *)
open Batteries
open BatNum
open BatIO
open Graphics
       
type point = num * num

type polygon = point list

type silhouette = polygon list

let parse_num (s : string) : num =
  try
    let s1, s2 = BatString.split s "/" in
    (of_string s1) / (of_string s2)
  with
  | Not_found -> of_string s
			    
			  
let parse_point (s : string) : point =
  let s1, s2 = BatString.split s "," in
  parse_num s1, parse_num s2
  

(* todo: skeleton *)
let parse_silouhette (filename : string) : silhouette =
  let f = open_in filename in
  let nb_poly = int_of_string (read_line f) in
  let silh = ref [] in
  for p = 1 to nb_poly do
    let poly = ref [] in
    let nb_vertices = int_of_string (read_line f) in
    for i = 1 to nb_vertices do
      print_endline "ta";
      let v = parse_point (read_line f) in
      poly := !poly @ [v]
    done;
    silh := !silh @ [!poly]
  done;
  !silh

  
	    

let is_positive (p : polygon) : bool =
  true


let width = 600
	      
let print_polygon (sat : num * num -> int * int) (p : polygon) : unit =
  if is_positive p then
    set_color red
  else set_color white;
  print_endline "t0";
  let p' = BatArray.of_list p in
  let p'' = Array.map sat p' in
  print_endline "t0";
  fill_poly p''

	      
let print_silhouette (s : silhouette) : unit =
  let points = List.concat s in
  let xs = List.map fst points in
  let ys = List.map snd points in
  let minx, maxx = BatList.min_max ~cmp:compare_num xs in
  let miny, maxy = BatList.min_max ~cmp:compare_num ys in
  let minx = min_num zero minx in
  let maxx = max_num one maxx in
  let miny = min_num zero miny in
  let maxy = max_num one maxy in
  let maxmax = max_num (maxx - minx) (maxy - miny) in
  let scale_and_translate (x,y : num * num) : int * int =
    to_int (approx (((x - minx) * of_int width) / maxmax)),
    to_int (approx (((y - miny) * of_int width) / maxmax))
  in
  open_graph "";
  set_window_title "youhou";
  resize_window width width;
  (* draw unit square *)
  set_color blue;
  let sq = [| zero, zero; one, zero; one, one; zero, one |] in
  fill_poly (Array.map scale_and_translate sq);
  (* draw the polygons *)
  List.iter (print_polygon scale_and_translate) s

(* let p' = Array.map (fun (x,y) -> to_int (x * of_int width), to_int (y * of_int width)) *)
		   (* (BatArray.of_list (BatList.hd s1)) *)
	     


let s1 = parse_silouhette "pb/ex"

let () =
  print_silhouette s1
