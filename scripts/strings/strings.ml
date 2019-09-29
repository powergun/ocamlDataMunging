#!/usr/bin/env ocaml

let runDemo () = 
  let c = String.get "Test" 3 in
    print_char(c);
  print_endline(String.make 9 'c');
  print_endline("there is " ^ "a cow");
  print_endline(String.concat "." ["there"; "is"; "a"; "cow"]);
  ;;
runDemo();;
