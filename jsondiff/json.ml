open Core

type value =
  Assoc
  | Bool of bool
  | Float of float
  | Int of int
  | List
  | Null
  | String of string
  | Assoc_Element of string
  | List_Element
[@@deriving sexp]

type lexing_position = Lexing.position
let sexp_of_lexing_position _ = sexp_of_int 5
let lexing_position_of_sexp _ = Lexing.dummy_pos

type location = {start_loc: lexing_position; end_loc: lexing_position}
[@@deriving sexp]

type tagged_value = {loc: unit; value: value} 
[@@deriving sexp]

let equal a b = match (a, b) with {value= va; _}, {value= vb; _} -> va = vb

let lex_compare : tagged_value -> tagged_value -> int =
 fun a b ->
  let cardinal_value a =
    match a with
    | Assoc -> 1
    | Bool _ -> 2
    | Float _ -> 3
    | Int _ -> 4
    | List -> 5
    | Null -> 6
    | String _ -> 7
    | Assoc_Element _ -> 8
    | List_Element -> 9
  in
  match (a, b) with {value= va; _}, {value= vb; _} -> (
    let diff = cardinal_value va - cardinal_value vb in
    if diff != 0 then diff
    else
      match (va, vb) with
      | Bool a', Bool b' -> compare a' b'
      | Float a', Float b' -> compare a' b'
      | Int a', Int b' -> compare a' b'
      | String a', String b' -> compare a' b'
      | Assoc_Element a', Assoc_Element b' -> compare a' b'
      | _ -> diff )
