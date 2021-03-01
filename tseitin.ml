
open Core

(* Figuring out how to implement the tseitin transform *)

type 'a formula =
	| Top
	| Bot
	| Nom of 'a
	| Neg of 'a formula
	| Min of 'a formula * 'a formula
	| Max of 'a formula * 'a formula
(*	| Gmb of 'a formula * 'a formula
	| Tov of 'a formula * 'a formula
	| Eql of 'a formula * 'a formula
	| Dif of 'a formula * 'a formula
	| Abv of 'a formula * 'a formula
	| Bel of 'a formula * 'a formula *)

(* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> *)
(* Printing for debugging *)
let print_list f l =
	let rec print_elements = function
		| [] -> ()
		| h::t -> f h ; print_string ";" ; print_elements t
	in
	print_string "["; print_elements l; print_string "]"

let printi i = Out_channel.output_string stdout (Int.to_string i)

(* <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> *)

let rec vars = function
	| Top | Bot -> 0
	| Nom _ -> 1
	| Neg o -> vars o
	| Max (l,r) | Min (l,r) -> vars l + (vars r)

let () = printi (vars (Max ((Nom 'l'), (Max ((Nom 'k'), (Nom 'l'))))))