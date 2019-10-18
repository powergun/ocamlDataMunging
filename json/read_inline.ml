open Core

(* 
heredoc in ocaml
https://rosettacode.org/wiki/Here_document#OCaml

*)
let json_blob = {json|
{
  "title": "Real World OCaml",
  "tags" : [ "functional programming", "ocaml", "algorithms" ],
  "pages": 450,
  "authors": [
    { "name": "Jason Hickey", "affiliation": "Google" },
    { "name": "Anil Madhavapeddy", "affiliation": "Cambridge"},
    { "name": "Yaron Minsky", "affiliation": "Jane Street"}
  ],
  "is_online": true
}
|json}

let%test "parse inline string, extract one field" =
  let json1 = Yojson.Basic.from_string json_blob in
  let open Yojson.Basic.Util in
  (* 
  mismatching keyname will cause exception
  *)
  Stdio.printf "title(%s)\n" (json1 |> member "title" |> to_string);

  true
;;

let%test "iterate over all objects" =
  let json1 = Yojson.Basic.from_string json_blob in
  let open Yojson.Basic.Util in
  (* 
  but the field is a list of strings instead of a single one. Converting 
  this to an OCaml string list is a two-stage process. First, we 
  convert the JSON List to an OCaml list of JSON values and then 
  filter out the String values as an OCaml string list
  *)
  let authors = json1 |> member "authors" |> to_list in
  let names = List.map authors ~f:(fun json -> member "name" json |> to_string) in
  List.iter names ~f:print_endline;
  
  true
;;

let%test "filter_string skips non-string objects" =
  (* 
  OCaml lists must contain values of the same type, so any JSON
  values that can not be converted to a string will be skipped 
  from the output of filter_string
  *)
  let json1 = Yojson.Basic.from_string "[null, [], \"iddqd\", 1]" in
  let open Yojson.Basic.Util in
  let strs = json1 |> to_list |> filter_string in
  List.iter strs ~f:print_endline;

  true
;;

let%test "optional field" =
  let json0 = Yojson.Basic.from_string "{}" in
  let json1 = Yojson.Basic.from_string "{\"name\": \"iddqd\"}" in
  let open Yojson.Basic.Util in
  begin 
    match (json0 |> member "name" |> to_string_option) with
    | None -> Stdio.printf "field(name) does not exist\n"
    | Some v -> Stdio.printf "field(name)=%s\n" v
  end;
  begin 
    match (json1 |> member "name" |> to_string_option) with
    | None -> Stdio.printf "field(name) does not exist\n"
    | Some v -> Stdio.printf "field(name)=%s\n" v
  end;
  true
;;
