open Core
open Lib
open Diff
open Jsondiff
open Io 

let open_exn v = match v with 
  | Some(v) -> v
  | None -> 
      print_endline "An error occurred while parsing the file. \n\
                     Either the file doesn't exist or its contents are not recognised."; 
      raise (Invalid_argument "not some") 

let jsondiff_test f1 f2=
  let lexbuf1 = Lexing.from_channel (In_channel.create f1) in
  let lexbuf2 = Lexing.from_channel (In_channel.create f2) in
  let t1 = open_exn (parse_with_error lexbuf1) in
  let t2 = open_exn (parse_with_error lexbuf2) in
  let ds = diff t1 t2 ~eq:Json.equal in
  printf "Diff = \n %s \n" (ds |> (sexp_of_script Json.sexp_of_tagged_value) |> Sexp.to_string_hum )

module CLI = Minicli.CLI

let main () =
  let argc, args = CLI.init () in
  if argc = 1 then
    (printf "usage:\n\
             %s {-s|--source} <file> {-t|--target} <file>" Sys.argv.(0); exit 1); 
  let f1 = CLI.get_string ["-s";"--source"] args in
  let f2 = CLI.get_string ["-t";"--target"] args in
  jsondiff_test f1 f2

let () = main ()
