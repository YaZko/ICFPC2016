open Batteries
open Num
open List
open Basics
open Myparsing
       
let is_convex (p : polygon) : bool =
  let verts = Array.of_list p in
  let n = Array.length verts in
  let res = ref true in
  for i = 1 to n do
    (* print_bool (ccw verts.(i mod n) verts.(pred i) verts.(succ i mod n)); *)
    res := !res && ccw verts.(i mod n) verts.(pred i) verts.(succ i mod n)
  done;
  !res

let is_convex_pb (s, sk : problem) : bool =
  match s with
  | [p] -> is_convex p
  | _ -> false

	   
let included_in_unit_square_point (px, py : point) : bool =
  zero <=/ px && px <=/ one && zero <=/ py && py <=/ one

let included_in_unit_square_poly (p : polygon) : bool =
  for_all included_in_unit_square_point p

let included_in_unit_square (s, sk : problem) : bool =
  for_all included_in_unit_square_poly s  


let () =
  for i = 1 to 2730 do
    try
      let pb = parse_problem ("../pb/" ^ (string_of_int i) ^ ".pb") in
      if is_convex_pb pb then
	if included_in_unit_square pb then
	  ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/convex/fit/" ^ (string_of_int i) ^ ".pb "))
	else
	  ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/convex/dontfit/" ^ (string_of_int i) ^ ".pb "))
      else
	ignore (Sys.command ("cp ../pb/" ^ (string_of_int i) ^ ".pb ../pb/notconvex/" ^ (string_of_int i) ^ ".pb "))
    with
    | _ -> ()
  done
