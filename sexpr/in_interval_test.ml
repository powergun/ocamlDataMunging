
open Core

let%test "basic Interval functionality" =
  let em = In_interval.create 1 0 in 
  let rng = In_interval.create 1 100 in

  assert(In_interval.is_empty em);
  assert(In_interval.contains rng 33);
  assert(not (In_interval.contains rng 12321));
  
  true
;;

let%test "Interval -> s-expr conversion" =
  let module I = In_interval in
  let intervals = [
    I.create 3 4;
    I.create 5 4;
    I.create 2 3;
    I.create 1 6;
  ] in

  intervals 
  |> List.sexp_of_t I.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline;

  true
;;

(* 
http://dev.realworldocaml.org/data-serialization.html
*)
let%test "S-expr -> Interval conversion" =
  let module I = In_interval in
  (* with the exception installed `Range 13 4` will cause an 
  exception *)
  let ss = {sexpression|
  (Empty 
  ;(Range 13 4)
  Empty)
  |sexpression} 
  in
  let sp = Sexp.of_string ss in
  let intervals = List.t_of_sexp I.t_of_sexp sp in

  List.iter intervals ~f:(fun rng -> assert(I.is_empty rng));

  true
;;
