

(* 
http://dev.realworldocaml.org/json.html
*)
let%test "create json list" = 
  let person = `Assoc [ 
    ("name", `String "doom");
    ("year", `Int 1993);
    ("platforms", `List [`String "dos"; `String "sfc"])
  ] in
  let s_person = Yojson.Basic.pretty_to_string person in
  Stdio.printf "%s\n" s_person;

  true
;;
