open Tree

type tai_map = TMatch of int * int | TDelete of int | TInsert of int
[@@deriving sexp]

type 'a deletion = Deletion of int [@@deriving sexp]

type 'a relabel =
  | Relabel of int * 'a * 'a
  (* Index * current value * target value *)
[@@deriving sexp]

type 'a insertion_tree =
  | IBranch of 'a * 'a insertion_tree list
  | ISkip of int
[@@deriving sexp]

type 'a rooted_insertion_tree = int * 'a insertion_tree list [@@deriving sexp]

type 'a script =
  'a relabel list * 'a deletion list * 'a rooted_insertion_tree list
[@@deriving sexp]

(* Calculates the minimum difference script between two nary trees *)
val diff : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> 'a script

val zs_tree_dist0 : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> int
val zs_tree_dist1 : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> int

val zs_tree_dist_and_map1 : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> int * tai_map list
val zs_tree_dist_and_map : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> int * tai_map list


(* Applies an edit script to the given tree *)
val apply_diff : 'a script -> 'a tree -> 'a tree
