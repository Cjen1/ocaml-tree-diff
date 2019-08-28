open Core
open Lib
open Tree


let gen_dels : 'a tree -> float -> 'a tree =
 fun tree p ->
  filtermapL tree
    ~filter:(fun _ -> true)
    ~f:(fun (Branch (v, cs)) ->
      Branch
        ( v
        , List.fold_right cs ~init:[] ~f:(fun (Branch (v, cs)) acc ->
              if Random.float 1.0 < p then cs @ acc else Branch (v, cs) :: acc
          ) ) )

let gen_rels : type a. a tree -> float -> (unit -> a) -> a tree =
 fun tree p rel ->
  filtermapL tree
    ~filter:(fun _ -> Random.float 1.0 < p)
    ~f:(fun (Branch (_, cs)) -> Branch (rel (), cs))

let gen_ins : type a. a tree -> float -> (unit -> a) -> a tree =
 fun tree p ins ->
  filtermapL tree
    ~filter:(fun _ -> Random.float 1.0 < p)
    ~f:(fun (Branch (v, cs)) ->
      match cs with _ ->
        let length = List.length cs in
        let a, b =
          match length with
          | 0 -> (0, 0)
          | _ -> (Random.int length, Random.int length)
        in
        let s, e = (min a b, max a b) in
        let cs' =
          let fst, snd = List.split_n cs s in
          let ccs, snd = List.split_n snd (e - s) in
          fst @ (Branch (ins (), ccs) :: snd)
        in
        Branch (v, cs') )

let string_gen () = char_of_int (97 + Random.int 26)

let gen_tree : 'a tree -> 'a tree =
 fun tree ->
  let p = 0.1 in
  let () = Random.self_init () in
  let minTree = gen_dels tree p in
  let relTree = gen_rels minTree p string_gen in
  let tree'   = gen_ins relTree p string_gen in
  tree'

let rec gen_size tree n =
  let size = Tree.size tree in
  if size < n then gen_size (gen_ins tree 0.1 string_gen) n else tree
