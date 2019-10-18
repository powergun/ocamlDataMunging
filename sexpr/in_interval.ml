
open Core

type t = Range of int * int
       | Empty
       [@@deriving sexp]

let is_empty = function
  | Empty -> true
  | Range _ -> false

let create x y = match (x > y) with
  | true -> Empty
  | false -> Range (x, y)
  
let contains rng x = match rng with
  | Empty -> false
  | Range (low, high) -> x >= low && x <= high

(* 
http://dev.realworldocaml.org/data-serialization.html

since t_of_sexp is defined with an ordinary let rather a `let rec`,
the call to the t_of_sexp goes to the Sexplib-generated version, 
rather than being a recursive call
*)
let t_of_sexp sexp = 
  let t = t_of_sexp sexp in
  begin match t with 
    | Empty -> ()
    | Range (x, y) -> 
      if y < x then
        of_sexp_error "Invalid range!" sexp
  end;
  t
