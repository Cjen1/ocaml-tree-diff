(* The main tree structure for diffing *)
type 'a tree = Branch of 'a * 'a tree list [@@deriving sexp]

(* 'a Tree with post order indexing *)
type 'a pi_tree = (int * 'a) tree [@@deriving sexp]

(* Gets the subtree from a particular postorder index *)
val get_ti_subtree : int -> 'a pi_tree -> 'a pi_tree 

val get : int -> 'a pi_tree -> 'a option

(* Converts a pi_tree into its equivalent association list, sorted by postorder index *)
val postordering_label_list : 'a pi_tree -> (int * 'a) list

val tree_map : 'a tree -> f:('a -> 'b) -> 'b tree

(* Converts a tree into its pi_tree form by tacking on postorder indices *)
val convert_to_postorder : 'a tree -> 'a pi_tree

(* Converts a tree into its pi_tree form by tacking on postorder indices *)
val convert_from_postorder : 'a pi_tree -> 'a tree

val tree_compare : 'a tree -> 'a tree -> compare:('a -> 'a -> int) -> int

(* finds nodes which match the filter and applies f to them *)
val filtermapL :
  filter:('a tree -> bool) -> f:('a tree -> 'a tree) -> 'a tree -> 'a tree

(* Folds over the structure of the tree *)
val fold : 'a tree -> f:('a -> 'b list -> 'b) -> 'b

val size : 'a tree -> int
