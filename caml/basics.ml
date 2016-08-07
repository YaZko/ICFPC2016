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
(* let string_of_segment (p, q : segment) = "(" ^ (string_of_point p) ^ ";" ^ (string_of_point q) ^ ")" *)
let string_of_segment (p, q : segment) = (string_of_point p) ^ " " ^ (string_of_point q)
let string_of_polygon (l : polygon) = "[" ^ String.concat ";" (map string_of_point l) ^ "]"
											 
let string_of_problem (sil, sk : problem) : string =
  let s = output_string () in
  write_line s (string_of_int (length sil));
  print ~first:"" ~last:"" ~sep:"\n"
	(fun out poly -> write_line out (string_of_int (length poly));
			 print  ~first:"" ~last:"\n" ~sep:"\n" (fun out point -> nwrite out (string_of_point point)) out poly)
	s sil;
  write_line s (string_of_int (length sk));
  print ~first:"" ~last:"\n" ~sep:"\n"
	(fun out seg -> nwrite out (string_of_segment seg)) s sk;
  close_out s


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
	      
let draw_polygon (sat : num * num -> int * int) (p : polygon) : unit =
  if is_positive p then
    set_color red
  else set_color white;
  let p' = Array.map sat (Array.of_list p) in
  fill_poly p'

let draw_segment (sat : num * num -> int * int) (s1, s2 : segment) : unit =
  set_color black;
  set_line_width 3;
  let x,y = sat s1 in
  let x', y' = sat s2 in
  draw_segments [| x, y, x', y' |]
	      
let draw_problem (s, sk : problem) : unit =
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
  List.iter (draw_polygon scale_and_translate) s;
  (* draw the skeleton *)
  List.iter (draw_segment scale_and_translate) sk	    
