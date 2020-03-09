
(* 
see also:
viet-ocaml-training/01_basics/o001_grep_lines.ml
*)

open Core

let%test "use character class" =
  (* 
  must pull "re" module in the dune libraries
  *)
  let regex = Re.Pcre.regexp {regex|\d+\.\d+|regex} in

  assert(Re.Pcre.pmatch ~rex:regex "127.0.0.1");
  assert(not (Re.Pcre.pmatch ~rex:regex "0xff7c"));

  true
;;

let prt_lines text =
  let rec aux = function
    | [] -> ()
    | hd :: tl -> 
      Stdio.printf "%s\n" hd;
      aux tl
  in
  aux (List.filter ~f:(fun l -> String.length l > 0) (String.split_lines text))
;;

(*
 * source: https://dune.readthedocs.io/en/stable/tests.html
 * inline expectation tests; this uses ppx_expect
 * see: https://github.com/janestreet/ppx_expect (this comes with jane street's)
 * *)
let%expect_test "unlines and print" =
  let text = {text|
; there is 
; a silence
; where hath been
; no sound
|text} in
  prt_lines text;
  [%expect{|
  ; there is
  ; a silence
  ; where hath been
  ; no sound
  |}]
;;

let grep_and_print rex text =
  let rec aux = function
    | [] -> ()
    | hd :: tl ->
      begin match (Re.Pcre.pmatch ~rex hd) with
        | false -> ()
        | true -> Stdio.printf "%s\n" hd
      end;
      aux tl
  in 
  aux (String.split_lines text)
;;

let%expect_test "unlines, grep (pcre) and print" =
  let rex = Re.Pcre.regexp {regex|0x[\da-fA-F]+|regex} in 
  let text = {text|
; comment
mov ecx 0x1212312fff
; there is acow
lea ebx ecx
; doom
push ebx
|text} in
  grep_and_print rex text;
  [%expect{|
  mov ecx 0x1212312fff
  |}]
;;
