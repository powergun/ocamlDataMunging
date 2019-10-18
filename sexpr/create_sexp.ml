open Core_kernel

let%test "create sexp using core kernel" =
  let sp = Sexp.List [
    Sexp.Atom "this";
    Sexp.List [ Sexp.Atom "is"; Sexp.Atom "an"];
    Sexp.List [ Sexp.Atom "s"; Sexp.Atom "expression" ];
  ] in
  let ss = Sexp.to_string sp in
  Stdio.printf "%s\n" ss;

  true
;;
