open Core
open Lib
open Tree
open Diff
open Utils

let integration_test () =
  let rec multitester n = if n = 0 then None else
      let r = Branch('r', []) in
      let diffsize = 20 in
      let t1 = Gentree.gen_size r diffsize in
      let t2 = Gentree.gen_tree t1 
      in
      let difference = diff 
          (apply_diff (diff t1 t2 ~eq:Char.equal) t1) 
          t2 
          ~eq:Char.equal in
      if difference = ([],[],[]) then multitester (n-1) else 
      (printf "%s \n%s" 
        (t1 |> Tree.sexp_of_tree Char.sexp_of_t |> Sexp.to_string_hum)
        (t2 |> Tree.sexp_of_tree Char.sexp_of_t |> Sexp.to_string_hum);
        Some(t1, t2))
  in match (multitester 10000) with
  | Some(_) -> print_endline "Integration test FAILED"; 
  | None -> print_endline "Intergration test successful"


let () = integration_test ()
