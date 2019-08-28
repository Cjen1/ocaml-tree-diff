open Core
open Lexer
open Lexing
open Json
open Lib.Tree

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      None
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)

let output_value (value : Json.value) : string =
  match value with
  | Assoc -> "Assoc"
  | Bool b -> string_of_bool b
  | Float f -> string_of_float f
  | Int i -> string_of_int i
  | List -> "List"
  | Null -> "Null"
  | Assoc_Element s -> "Assoc: " ^ s
  | List_Element -> "List"
  | String s -> "String: " ^ s

let print_tagged_tree tree =
  let rec iter (Branch ({value; _}, cs)) tab =
    print_endline (tab ^ output_value value) ;
    let rec list_iter cs =
      match cs with
      | [] -> ()
      | c :: cs' ->
          iter c (tab ^ "\t") ;
          list_iter cs'
    in
    list_iter cs
  in
  iter tree ""
