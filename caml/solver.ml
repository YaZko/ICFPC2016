open Batteries
open Num
open List
open Basics
open Myparsing

let id_pb : problem = parse_problem "../pb/identity.pb"
let id_sol : solution =
  match id_pb with
  | [points], _ -> points, points, [[0; 1; 2; 3]]
  | _ -> assert false

type vector = num * num

let scalar (ux, uy : point) (vx, vy : point) : num =
  ux * vx + uy * vy

let mult (a : num) (vx, vy : vector) : vector =
  a * vx, a * vy

let squared_norm (vx, vy : vector) =
  vx * vx + vy * vy

let two = one + one

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
  assert (eq_num (squared_norm (vx, vy)) one);
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

let squared_norm_seg ((p1x, p1y), (p2x, p2y) : segment) =
  (p2x - p1x) * (p2x - p1x) + (p2y - p1y) * (p2y - p1y)
	  
let unit_length_segments_polygon (p : polygon) : segment list =
  let segs = ref [] in
  let verts = Array.of_list p in
  let n = Array.length verts in
  for i = 0 to pred n do
    if equal (squared_norm_seg (verts.(i), verts.(succ i mod n))) one then
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
	   
let included_in_unit_square_point (px, py : point) : bool =
  zero <=/ px && px <=/ one && zero <=/ py && py <=/ one

let included_in_unit_square_poly (p : polygon) : bool =
  for_all included_in_unit_square_point p

let included_in_unit_square (s, sk : problem) : bool =
  for_all included_in_unit_square_poly s  

	  
let solve_by_translation_rotate (pb : problem) : pb_transformer =
  let segs = unit_length_segments (fst pb) in
  let transfs = map (fun (p1, p2) ->
		     comp (rotate  (minus p2 p1)) (translate (inv p1)))
		    segs in
  find (fun (pb_tr, _) -> is_id_pb (pb_tr pb)) transfs


let fit_by_translation_rotate (pb : problem) : pb_transformer =
  let segs = unit_length_segments (fst pb) in
  let transfs = map (fun (p1, p2) ->
		     comp (rotate  (minus p2 p1)) (translate (inv p1)))
		    segs in
  find (fun (pb_tr, _) -> included_in_unit_square (pb_tr pb)) transfs
  
  (* print_int (length transfs); print_endline " ..."; *)
  (* print_string (string_of_polygon (hd (fst (hd (map fst pbs'))))); *)

(* let sols = filter (fun (pb_tr, sol_tr) -> is_id_pb (pb_tr pb)) transfs in *)
  (* (\* print_int (length sols); print_endline " ..."; *\) *)
  (* match sols with *)
  (* | (_, sol_tr) :: _ -> Some (sol_tr id_sol) *)
  (* | _ -> None *)
  (* (\* snd (hd sols) id_sol *\) *)
  

let is_convex (p : polygon) : bool =
  let verts = Array.of_list p in
  let n = Array.length verts in
  let res = ref true in
  for i = 1 to n do
    res := !res && ccw verts.(i mod n) verts.(pred i) verts.(succ i mod n)
  done;
  !res

let is_convex_pb (s, sk : problem) : bool =
  match s with
  | [p] -> is_convex p
  | _ -> false


let translate_left_bottom_on_zero (s, sk : problem) : pb_transformer =
  let points = concat s in
  let minx, _ = min_max ~cmp:compare_num (map fst points) in
  let miny, _ = min_max ~cmp:compare_num (map snd points) in
  translate (inv (minx, miny))


(* let convex_hull (s, _ : problem) : polygon = *)
(*   let points = concat s in *)
(*   let stack = Stack.create () in *)
	   
	    
let () =
  let pb_max = 5009 in
  let sol_trs = Array.make (succ pb_max) (fun s -> s) in
  let todo = Array.make (succ pb_max) false in
  for i = 1 to pb_max do
    try
      let pb = parse_problem ("../pb/" ^ (string_of_int i) ^ ".pb") in
      try
	let pb_tr, sol_tr = solve_by_translation_rotate pb in
	(* ignore (Sys.command ("mv ../pb/" ^ (string_of_int i) ^ ".pb ../pb/solved/" ^ (string_of_int i) ^ ".pb ")); *)
	File.write_lines ("../pb/" ^ (string_of_int i) ^ ".sol") (Enum.singleton (string_of_solution (sol_tr id_sol)))
      with
      | Not_found -> (
	if is_convex_pb pb then
	  if included_in_unit_square pb then (
	    todo.(i) <- true;
	    ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/convex/fit/" ^ (string_of_int i) ^ ".pb "))
	  ) else
	    try
	      let pb_tr, sol_tr = fit_by_translation_rotate pb in
	      todo.(i) <- true;
	      sol_trs.(i) <- sol_tr;
	      File.write_lines ("../pb/convex/fit/" ^ (string_of_int i) ^ ".pb") (Enum.singleton (string_of_problem (pb_tr pb)))
	    with
	    | Not_found ->
	       let pb_tr, sol_tr = translate_left_bottom_on_zero pb in
	       if included_in_unit_square (pb_tr pb) then (
		 todo.(i) <- true;
		 sol_trs.(i) <- sol_tr;
		 File.write_lines ("../pb/convex/fit/" ^ (string_of_int i) ^ ".pb") (Enum.singleton (string_of_problem (pb_tr pb)))
	       ) else
		 ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/convex/dontfit/" ^ (string_of_int i) ^ ".pb "))
	else
	  ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/notconvex/" ^ (string_of_int i) ^ ".pb "))
      )
    with
    | _ -> ()
  done;
  print_endline "ok? (press enter)";
  ignore (read_line ());
  for i = 1 to pb_max do
    if todo.(i) then
      let yaya_sol = parse_solution ("../pb/yaya/" ^ (string_of_int i) ^ ".sol") in
      File.write_lines ("../pb/" ^ (string_of_int i) ^ ".sol") (Enum.singleton (string_of_solution (sol_trs.(i) yaya_sol)))
  done


  



  
  (* print_int (length !pbs); print_newline (); *)
  (* print_int (length (filter_map try_rotate (map snd !pbs))); print_newline (); *)
  
  (* let pb', transf = translate (inv (left_bottom_point (fst pb))) pb in *)
  (* print_string (string_of_solution (transf id_sol));; *)
  (* print_string (string_of_solution (try_rotate pb)) *)



	       
(* type point = num * num *)
(* type segment = point * point *)
			 
(* type polygon = point list *)

(* type silhouette = polygon list *)
(* type skeleton = segment list *)

(* type problem = silhouette * skeleton *)
(* type solution = (point list) * (point list) * (int list list) *)
