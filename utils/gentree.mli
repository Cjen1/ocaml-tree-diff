open Lib
open Tree

val string_gen : unit -> char

val gen_size : char tree -> int -> char tree

val gen_dels : 'a tree -> float -> 'a tree

val gen_rels : 'a tree -> float -> (unit -> 'a) -> 'a tree

val gen_ins : 'a tree -> float -> (unit -> 'a) -> 'a tree

val gen_tree : char tree -> char tree
