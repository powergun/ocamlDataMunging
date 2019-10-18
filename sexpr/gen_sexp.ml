
open Core

type t = { foo: int
         ; bar: float } 
         [@@deriving sexp]

let%test "deserialize string to create s-expression" = 
  let r = t_of_sexp (Sexp.of_string "((bar 35) (foo 3))") in
  Stdio.printf "record: %d %0.2f\n" r.foo r.bar;

  true
;;

let%test "serialize using annoymous type" =
  let l = [(1,"one"); (2,"two")] in
  List.iter l ~f:(fun x -> 
    [%sexp_of: int * string] x
    |> Sexp.to_string
    |> print_endline
  );

  true
;;