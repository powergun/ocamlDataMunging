open Core_kernel

let%test "create sexp using core kernel" =
  let sp = Sexp.List [
    Sexp.Atom "this";
    Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an"];
    Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ];
  ] in
  let ss = Sexp.to_string sp in
  Stdio.printf "%s\n" ss;

  let sp' = Sexp.of_string ss in
  Stdio.printf "roundtrip: %b\n" (sp' = sp);

  true
;;

(* 
most of the base types in Core support conversion to and from 
s-expressions
*)
let%test "convert built-in types to s-exp" = 
  let t = "asd" in
  let sp = String.sexp_of_t t in
  let ss = Sexp.to_string sp in
  Stdio.printf "%s\n" ss;
  
  (* 
  Notice that List.sexp_of_t is polymorphic and takes as its first 
  argument another conversion function to handle the elements of the 
  list to be converted. Core uses this scheme more generally for 
  defining sexp converters for polymorphic types.
  *)
  let t = [1; 2; 3] in
  let sp = List.sexp_of_t Int.sexp_of_t t in
  let ss = Sexp.to_string sp in
  Stdio.printf "%s\n" ss;

  let t' = List.t_of_sexp Int.t_of_sexp (Sexp.of_string ss) in
  Stdio.printf "roundtrip: %b\n" (t = t');

  true
;;




