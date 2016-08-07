(* OCaml toplevel to run: ocaml graphics.cma images.cmo *)
open Batteries
open Num
open IO
open Graphics
open List
open Basics
	    
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

let parse_solution (filename : string) : solution =
  let f = open_in filename in
  (* parse points *)
  let nb_points = int_of_string (read_line f) in
  let points = ref [] in
  for p = 1 to nb_points do
    let v = parse_point (read_line f) in
    points := !points @ [v]
  done;
  (* parse facets *)
  let nb_facets = int_of_string (read_line f) in
  let facets = ref [] in
  for p = 1 to nb_facets do
    let s = read_line f in
    let l = String.nsplit s " " in
    facets := !facets @ [map int_of_string (tl l)]
  done;
  (* parse dests *)
  let dests = ref [] in
  for p = 1 to nb_points do
    let v = parse_point (read_line f) in
    dests := !dests @ [v]
  done;
  !points, !dests, !facets
  
