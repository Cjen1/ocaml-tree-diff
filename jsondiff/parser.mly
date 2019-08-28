%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%{
  open Lib
  open Core

  let mk_tagged_value: Json.value -> (Lexing.position * Lexing.position) -> Json.tagged_value =  
    fun value (start_pos, end_pos) -> {
        (*loc= {
          start_loc = start_pos; 
          end_loc = end_pos
        }*)
        loc = (); 
        value = value
      }
%}

%start <Json.tagged_value Lib.Tree.tree option> prog
%%

prog:
  | EOF { None }
  | v = value { Some v }
;

value:
  | LEFT_BRACK; vl = array_fields; RIGHT_BRACK
    { Tree.Branch(mk_tagged_value (List) $sloc, vl) }
  | LEFT_BRACE; obj = obj_fields; RIGHT_BRACE
    { Tree.Branch(mk_tagged_value (Assoc) $sloc, obj) }
  | s = STRING
    { Tree.Branch(mk_tagged_value (String s) $sloc, []) }
  | i = INT
    { Tree.Branch(mk_tagged_value (Int i) $sloc, []) }
  | x = FLOAT
    { Tree.Branch(mk_tagged_value (Float x) $sloc, []) }
  | TRUE
    { Tree.Branch(mk_tagged_value (Bool true) $sloc, []) }
  | FALSE
    { Tree.Branch(mk_tagged_value (Bool false) $sloc, []) }
  | NULL
    { Tree.Branch(mk_tagged_value (Null) $sloc, []) }
  ;

array_fields:
  obj = separated_list(COMMA, value)        { obj }

obj_fields:
  obj = separated_list(COMMA, obj_field)    
    { 
      List.sort 
        ~compare:(
          fun a b -> 
            (
              Tree.tree_compare 
                a 
                b 
                ~compare:(
                  Json.lex_compare
                )
            )
        )
        obj 
    }

obj_field:
  k = STRING; COLON; v = value              { Tree.Branch(mk_tagged_value (Assoc_Element(k)) $sloc, [v]) }

