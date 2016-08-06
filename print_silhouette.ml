(* OCaml toplevel to run: ocaml graphics.cma images.cmo *)
open Batteries
open Num
open IO
open Graphics
open List
       
type point = num * num
type segment = point * point
			 
type polygon = point list

type silhouette = polygon list
type skeleton = segment list

type problem = silhouette * skeleton
type solution = (point list) * (point list) * (int list list)

let string_of_point (x, y : point) = (Num.to_string x) ^ "," ^ (Num.to_string y)
let string_of_segment (p, q : segment) = "(" ^ (string_of_point p) ^ ";" ^ (string_of_point q) ^ ")"
let string_of_polygon (l : polygon) = "[" ^ String.concat ";" (map string_of_point l) ^ "]"
											 
(* let string_of_problem (s, sk  *)

let string_of_solution (points, dests, facets : solution) : string =
  let s = output_string () in
  write_line s (string_of_int (length points));
  print ~first:"" ~last:"\n" ~sep:"\n"
	(fun out point -> nwrite out (string_of_point point)) s points;
  write_line s (string_of_int (length facets));
  print ~first:"" ~last:"\n" ~sep:"\n"
	(fun out poly ->
	 nwrite s (string_of_int (length poly));
	 print ~first:" " ~last:"" ~sep:" "
	       (fun out n -> nwrite out (string_of_int n)) out poly)
	s facets;
  print ~first:"" ~last:"\n" ~sep:"\n"
	(fun out point -> nwrite out (string_of_point point)) s dests;
  close_out s

	    
let parse_num (s : string) : num =
  try
    let s1, s2 = String.split s "/" in
    (of_string s1) / (of_string s2)
  with
  | Not_found -> of_string s
			    
let parse_point (s : string) : point =
  let s1, s2 = String.split s "," in
  parse_num s1, parse_num s2

let parse_segment (s : string) : segment =
  let s1, s2 = String.split s " " in
  parse_point s1, parse_point s2
  

let parse_problem (filename : string) : problem =
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


let det (x1,y1 : point) (x2,y2 : point) =
  x1 * y2 - x2 * y1

(* if ab, ac is direct (angle between 0 and pi) *)
let ccw (xa, ya : point) (xb, yb : point) (xc, yc : point) : bool =
  ((xb - xa) * (yc - ya) - (xc - xa) * (yb - ya)) <=/ zero


let succ = Pervasives.succ
let pred = Pervasives.pred

let is_positive (p : polygon) : bool =
  let verts = Array.of_list p in
  let n = Array.length verts in
  let minx, _ = min_max ~cmp:compare_num (map fst p) in
  let p' = filter (equal minx % fst) p in
  let _, maxy = min_max ~cmp:compare_num (map snd p') in
  let i = hd (filteri_map
		(fun i (x, y) -> if equal x minx && equal y maxy then Some i else None) p) in
  ccw verts.(i) verts.(pred (Pervasives.(+) i n) mod n) verts.(succ i mod n)
    

let width = 950
	      
let print_polygon (sat : num * num -> int * int) (p : polygon) : unit =
  if is_positive p then
    set_color red
  else set_color white;
  let p' = Array.map sat (Array.of_list p) in
  fill_poly p'

let print_segment (sat : num * num -> int * int) (s1, s2 : segment) : unit =
  set_color black;
  set_line_width 3;
  let x,y = sat s1 in
  let x', y' = sat s2 in
  draw_segments [| x, y, x', y' |]
	      
let print_problem (s, sk : problem) : unit =
  let points = concat s in
  let xs = map fst points in
  let ys = map snd points in
  let minx, maxx = min_max ~cmp:compare_num xs in
  let miny, maxy = min_max ~cmp:compare_num ys in
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
  List.iter (print_segment scale_and_translate) sk
	    
(* let () = *)
(*   open_graph ""; *)
(*   set_window_title "youhou"; *)
(*   resize_window width width; *)
(*   for i = 228 to 690 do *)
(*     clear_graph (); *)
(*     print_problem (parse_problem ("pb/" ^ (string_of_int i) ^ ".pb")); *)
(*     let img = get_image 0 0 width width in *)
(*     Images.sauver_image (dump_image img) ("pb/" ^ (string_of_int i) ^ ".png"); *)
(*   done; *)
(*   close_graph () *)




let id_pb : problem = parse_problem "pb/identity.pb"
let id_sol : solution =
  let [points], _ = id_pb in
  points, points, [[0; 1; 2; 3]]

type vector = num * num

let inv (vx, vy : vector) : vector =
  neg vx, neg vy
	    
let translate_point (vx, vy : vector) (px, py : point) : point =
  px + vx, py + vy

let minus (p2 : point) (p1 : point) : vector =
  translate_point (inv p1) p2
		  
let translate_segment (v : vector) (p1, p2 : segment) : segment =
  translate_point v p1, translate_point v p2
    
let translate_polygon (v : vector) (p : polygon) : polygon =
  map (translate_point v) p


type pb_transformer = (problem -> problem) * (solution -> solution)
      
let translate (v : vector) : pb_transformer =
  let pb_tr (s, sk) =
    map (translate_polygon v) s, map (translate_segment v) sk in
  let sol_tr (points, dests, facets : solution) =
    points, (map (translate_point (inv v)) dests), facets
  in pb_tr, sol_tr

(* rotation around the origin such that v goes on the x axe *)
let rotate_point (vx, vy : vector) (px, py : point) : point =
  let l = of_float (Float.sqrt (to_float (vx * vx + vy * vy))) in
  (vx * px + vy * py) / l, (vx * py - vy * px) / l

let rotate_segment (v : vector) (p1, p2 : segment) : segment =
  rotate_point v p1, rotate_point v p2
    
let rotate_polygon (v : vector) (p : polygon) : polygon =
  map (rotate_point v) p

let symx (vx, vy : vector) : vector =
  vx, neg vy
      
let rotate (v : vector) : pb_transformer =
  let pb_tr (s, sk) =
    map (rotate_polygon v) s, map (rotate_segment v) sk in
  let sol_tr (points, dests, facets : solution) =
    points, (map (rotate_point (symx v)) dests), facets
  in pb_tr, sol_tr
	    
let left_bottom_point (s : silhouette) : point =
  let points = concat s in
  let minx, _ = min_max ~cmp:compare_num (map fst points) in
  let points' = filter (equal minx % fst) points in
  let miny, _ = min_max ~cmp:compare_num (map snd points') in
  minx, miny

let squared_norm ((p1x, p1y), (p2x, p2y) : segment) =
  (p2x - p1x) * (p2x - p1x) + (p2y - p1y) * (p2y - p1y)
	  
let unit_length_segments_polygon (p : polygon) : segment list =
  let segs = ref [] in
  let verts = Array.of_list p in
  let n = Array.length verts in
  for i = 0 to pred n do
    if equal (squared_norm (verts.(i), verts.(succ i mod n))) one then
      segs := !segs @ [verts.(i), verts.(succ i mod n); verts.(succ i mod n), verts.(i)]
  done;
  !segs
  
let unit_length_segments (s : silhouette) : segment list =
  concat (map unit_length_segments_polygon s)
  
	    
let is_unit_square (p : polygon) =
  eq (Tuple2.eq eq_num eq_num) p (hd (fst id_pb))
	 
let is_id_pb (s, sk : problem) =
  match s with
  | [p] -> is_unit_square p
  | _ -> false

let comp (t2 : pb_transformer) (t1 : pb_transformer) : pb_transformer
  = (fst t2 % fst t1), (snd t1 % snd t2)
	   

let try_rotate (pb : problem) : solution =
  let segs = unit_length_segments (fst pb) in
  let transfs = map (fun (p1, p2) ->
		     comp (rotate  (minus p2 p1)) (translate (inv p1)))
		    segs in
  print_int (length transfs); print_endline " ...";
  (* print_string (string_of_polygon (hd (fst (hd (map fst pbs'))))); *)
  let sols = filter (fun (pb_tr, sol_tr) -> is_id_pb (pb_tr pb)) transfs in
  print_int (length sols); print_endline " ...";
  snd (hd sols) id_sol
  


let () =
  let pb = parse_problem "pb/102.pb" in

  (* let pb', transf = translate (inv (left_bottom_point (fst pb))) pb in *)
  (* print_string (string_of_solution (transf id_sol));; *)
  print_string (string_of_solution (try_rotate pb))



	       
(* type point = num * num *)
(* type segment = point * point *)
			 
(* type polygon = point list *)

(* type silhouette = polygon list *)
(* type skeleton = segment list *)

(* type problem = silhouette * skeleton *)
(* type solution = (point list) * (point list) * (int list list) *)
