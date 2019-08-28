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

type location = {start_loc: Lexing.position; end_loc: Lexing.position}
[@@deriving sexp]

type tagged_value = {loc: unit; value: value}
[@@deriving sexp]

val equal : tagged_value -> tagged_value -> bool

val lex_compare : tagged_value -> tagged_value -> int
