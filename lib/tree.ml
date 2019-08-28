open Core

type 'a tree = Branch of 'a * 'a tree list [@@deriving sexp]

type 'a pi_tree = (int * 'a) tree [@@deriving sexp]

let convert_to_postorder tree : 'a pi_tree =
  let rec iter tree n subtrees_accum =
    match tree with
    | Branch (v, []) -> (n + 1, Branch ((n + 1, v), List.rev subtrees_accum))
    | Branch (v, tl :: trs) ->
        let nl, ptl = iter tl n [] in
        iter (Branch (v, trs)) nl (ptl :: subtrees_accum)
  in
  let _, pt = iter tree 0 [] in
  pt

let rec get_ti_subtree (ti : int) (tree : 'a pi_tree) : 'a pi_tree  =
  match tree with
  | Branch ((i, _), _) when i = ti -> tree
  | Branch (v, (Branch((ia,_),_) as st)::cs) -> 
      if ia = ti 
      then st
      else if ti < ia
      then get_ti_subtree ti st
      else get_ti_subtree ti (Branch(v, cs))
  | _ -> raise (Invalid_argument ("Index " ^ (string_of_int ti) ^ " not found in tree"))

let rec get (ti : int) (tree : 'a pi_tree) : 'a option =
  match tree with
  | Branch ((i, v), _) when i = ti -> Some v
  | Branch (_, []) -> None
  | Branch (v, x :: xs) -> (
    match get ti x with
    | Some tree -> Some tree
    | None -> get ti (Branch (v, xs)) )

let postordering_label_list (tree : 'a pi_tree) =
  let rec helper tree accum =
    match tree with
    | Branch (v, []) -> v :: accum
    | Branch (v, xs) ->
        v :: List.fold xs ~init:accum ~f:(fun accum t -> helper t accum)
  in
  List.rev (helper tree [])

let tree_map tree ~f =
  let rec to_map (Branch (v, xs)) = Branch (f v, List.map xs ~f:to_map) in
  to_map tree

let convert_from_postorder tree = tree_map tree ~f:(fun (_, v) -> v)

let rec tree_compare (Branch (va, acs)) (Branch (vb, bcs)) ~compare =
  let diff = compare va vb in
  match diff with
  | 0 -> (
    match
      List.fold acs ~init:(None, bcs) ~f:(fun (found, bcs') a' ->
          match found with
          | Some diff' -> (Some diff', [a'])
          | None -> (
            match bcs' with
            | [] -> (* A longer than B thus A > B *) (Some 1, [])
            | b' :: bcs'' -> (
                let diff' = tree_compare a' b' ~compare in
                match diff' with 0 -> (None, bcs'') | _ -> (Some diff', bcs'')
                ) ) )
    with
    | Some diff', _ -> diff'
    | None, [] -> 0
    | None, _ -> -1 )
  | _ -> diff

let rec filtermapL :
    filter:('a tree -> bool) -> f:('a tree -> 'a tree) -> 'a tree -> 'a tree =
 fun ~filter ~f tree ->
  let (Branch (v, cs)) = if filter tree then f tree else tree in
  Branch (v, List.map ~f:(filtermapL ~filter ~f) cs)

let rec fold (Branch(v, cs)) ~f = 
  f v (List.map cs ~f:(fun st -> fold st ~f))

let rec size : 'a tree -> int =
 fun (Branch (_, cs)) ->
  1 + List.fold cs ~init:0 ~f:(fun acc st -> acc + size st)
