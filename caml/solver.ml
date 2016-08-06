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
