open Core
open Tree

type tai_map = TMatch of int * int | TDelete of int | TInsert of int
[@@deriving sexp]

(* Gets all unique calculations required for zs tree dist.
 * Returns all nodes *)
let lr_keyroots (Branch ((i, _), _) as tree) =
  let rec helper tree keyroots =
    match tree with
    | Branch (_, []) -> keyroots
    | Branch (_, x :: xs) ->
        let keyroots1 = helper x keyroots in
        List.fold xs ~init:keyroots1
          ~f:(fun keyroots (Branch ((si, _), _) as t) ->
            helper t (si :: keyroots) )
  in
  List.sort ~compare:Int.compare (helper tree [i])

let postorder_lookup (Branch((_,v),_) as t : 'a pi_tree) = 
  let t_size = Tree.size(t) in
  let arr = Array.create ~len:(t_size + 1) v in
  let _ = Tree.fold t ~f:(fun (i,v) _ -> Array.set arr i v) in
  fun i -> Array.get arr i

let leaf_lookup (t : 'a pi_tree) = 
  let t_size = Tree.size(t) in
  let arr = Array.create ~len:(t_size + 1) 0 in
  let _ = Tree.fold t ~f:(fun (i,_) fcs -> match fcs with
    | [] -> Array.set arr i i; i
    | c::_ -> Array.set arr i c; c
  ) in
  fun i -> Array.get arr i

let zs_tree_dist0 t1 t2 ~eq = 
  printf "ZS Main, |T1| = %d, |T2| = %d \n" (Tree.size(t1)) (Tree.size(t2)); 
  let (Branch((tt1,_),_) as pt1), (Branch((tt2,_),_) as pt2) = Tree.convert_to_postorder t1, Tree.convert_to_postorder t2 in
  let lookup1, lookup2 = postorder_lookup pt1, postorder_lookup pt2 in
  let l1, l2 = leaf_lookup pt1, leaf_lookup pt2 in
  let k1, k2 = lr_keyroots pt1, lr_keyroots pt2 in
  let gamma o = match o with
  | Some i, Some j  -> if eq (lookup1 i) (lookup2 j) then 0 else 1
  | Some _, None    -> 1
  | None,   Some _  -> 1
  | None,   None    -> assert false
  in
  let d = Hashtbl.Poly.create () in
  let d_get, d_set = (
    (fun (i,j) ->   Hashtbl.find_exn d (i, j)), 
    (fun (i,j) v -> Hashtbl.set d ~key:(i,j) ~data:v))
  in
  let populate_forest_dist_matrix x y =
    let fd = Hashtbl.Poly.create() in
    let n1, n2 = (l1 x) - 1, (l2 y) - 1 in
    let fd_get, fd_set = (
      (fun (i,j) ->   Hashtbl.find_exn fd (i, j)), 
      (fun (i,j) v -> Hashtbl.set fd ~key:(i,j) ~data:v))
    in
    let () = fd_set (n1,n2) (0) in
    for i = l1 x to x do
      (fd_set (i, n2) ((fd_get (i-1, n2)) + (gamma (Some(i), None))))
    done;
    for j = l2 y to y do 
      (fd_set (n1, j) ((fd_get (n1, j-1)) + (gamma (None, Some(j)))))
    done; 
    for i = l1 x to x do 
      for j = l2 y to y do 
        let o1 = ((fd_get (i-1, j)) + (gamma (Some(i), None))) in
        let o2 = ((fd_get (i, j-1)) + (gamma (None, Some(j)))) in
        if l1 i = l1 x && l2 j = l2 y
        then 
          let o3 = (fd_get (i-1,j-1)) + (gamma(Some(i), Some(j))) in
          let c = min o1 @@ min o2 o3 in
          fd_set (i,j) c;
          d_set (i,j) c
        else
          let o3 = (
            let cf =
              fd_get ((l1 i)-1, (l2 j)-1)
            and ct = d_get (i, j) in
            (cf + ct)
          )
          in
          fd_set (i,j) (min o1 @@ min o2 o3 )
      done
    done;
  in
  (List.iter k1 ~f:(fun x ->
      List.iter k2 ~f:(fun y ->
        populate_forest_dist_matrix x y
      )
    )
  );
  (d_get (tt1, tt2))

let zs_tree_dist1 t1 t2 ~eq = 
  printf "ZS Main, |T1| = %d, |T2| = %d \n" (Tree.size(t1)) (Tree.size(t2)); 
  let (Branch((tt1,_),_) as pt1), (Branch((tt2,_),_) as pt2) = Tree.convert_to_postorder t1, Tree.convert_to_postorder t2 in
  let lookup1, lookup2 = postorder_lookup pt1, postorder_lookup pt2 in
  let l1, l2 = leaf_lookup pt1, leaf_lookup pt2 in
  let k1, k2 = lr_keyroots pt1, lr_keyroots pt2 in
  let gamma o = match o with
  | Some i, Some j  -> if eq (lookup1 i) (lookup2 j) then 0 else 1
  | Some _, None    -> 1
  | None,   Some _  -> 1
  | None,   None    -> assert false
  in
  let ts1, ts2 = Tree.size t1, Tree.size t2 in
  let d = Array.create ~len:((ts1 + 1) * (ts2 + 1)) 0 in
  let d_get, d_set = (
    (fun (i,j) ->   Array.get d (i*(ts2 + 1) + j)),
    (fun (i,j) v -> Array.set d (i*(ts2 + 1) + j) v)
  )
  in
  let fd = Array.create ~len:((ts1 + 1) * (ts2 + 1)) 0 in
  let fd_get, fd_set = (
    (fun (i,j) ->   Array.get fd (i*(ts2 + 1) + j)),
    (fun (i,j) v -> Array.set fd (i*(ts2 + 1) + j) v)
  )
  in
  let populate_forest_dist_matrix x y =
    let n1, n2 = (l1 x) - 1, (l2 y) - 1 in
    let () = fd_set (n1,n2) (0) in
    for i = l1 x to x do
      (fd_set (i, n2) ((fd_get (i-1, n2)) + (gamma (Some(i), None))))
    done;
    for j = l2 y to y do 
      (fd_set (n1, j) ((fd_get (n1, j-1)) + (gamma (None, Some(j)))))
    done; 
    for i = l1 x to x do 
      for j = l2 y to y do 
        let o1 = ((fd_get (i-1, j)) + (gamma (Some(i), None))) in
        let o2 = ((fd_get (i, j-1)) + (gamma (None, Some(j)))) in
        if l1 i = l1 x && l2 j = l2 y
        then 
          let o3 = (fd_get (i-1,j-1)) + (gamma(Some(i), Some(j))) in
          let c = min o1 @@ min o2 o3 in
          fd_set (i,j) c;
          d_set (i,j) c
        else
          let o3 = (
            let cf =
              fd_get ((l1 i)-1, (l2 j)-1)
            and ct = d_get (i, j) in
            (cf + ct)
          )
          in
          fd_set (i,j) (min o1 @@ min o2 o3 )
      done
    done;
  in
  (List.iter k1 ~f:(fun x ->
      List.iter k2 ~f:(fun y ->
        populate_forest_dist_matrix x y
      )
    )
  );
  d_get (tt1, tt2)

let zs_tree_dist_and_map1 t1 t2 ~eq = 
  printf "ZS Main, |T1| = %d, |T2| = %d \n" (Tree.size(t1)) (Tree.size(t2)); 
  let cmin (i,vi) (j,vj) = if i < j then (i,vi) else (j,vj) in 
  let (Branch((tt1,_),_) as pt1), (Branch((tt2,_),_) as pt2) = Tree.convert_to_postorder t1, Tree.convert_to_postorder t2 in
  let lookup1, lookup2 = postorder_lookup pt1, postorder_lookup pt2 in
  let l1, l2 = leaf_lookup pt1, leaf_lookup pt2 in
  let k1, k2 = lr_keyroots pt1, lr_keyroots pt2 in
  let ts1, ts2 = Tree.size t1, Tree.size t2 in
  let d = Array.create ~len:((ts1 + 1) * (ts2 + 1)) (0,[]) in
  let d_get, d_set = (
    (fun (i,j) ->   Array.get d (i*(ts2 + 1) + j)),
    (fun (i,j) v -> Array.set d (i*(ts2 + 1) + j) v)
  )
  in
  let fd = Array.create ~len:((ts1 + 1) * (ts2 + 1)) (0,[]) in
  let fd_get, fd_set = (
    (fun (i,j) ->   Array.get fd (i*(ts2 + 1) + j)),
    (fun (i,j) v -> Array.set fd (i*(ts2 + 1) + j) v)
  )
  in
  let seq (d,m) o = match o with
  | Some i, Some j  -> (d+(if eq (lookup1 i) (lookup2 j) then 0 else 1), TMatch(i,j)::m)
  | Some i, None    -> (d+1,TDelete(i)::m)
  | None,   Some j  -> (1,TInsert(j)::m)
  | None,   None    -> assert false
  in
  let populate_forest_dist_matrix x y =
    let n1, n2 = (l1 x) - 1, (l2 y) - 1 in
    let () = fd_set (n1,n2) (0,[]) in
    for i = l1 x to x do
      (fd_set (i, n2) (seq (fd_get (i-1, n2)) (Some(i), None)))
    done;
    for j = l2 y to y do 
      (fd_set (n1, j) (seq (fd_get (n1, j-1)) (None, Some(j))))
    done; 
    for i = l1 x to x do 
      for j = l2 y to y do 
        let o1 = seq (fd_get (i-1,j)) (Some(i), None) in
        let o2 = seq (fd_get (i,j-1)) (None, Some(i)) in
        if l1 i = l1 x && l2 j = l2 y
        then 
          let o3 = seq (fd_get (i-1,j-1)) (Some(i), Some(j)) in 
          let c = cmin o1 @@ cmin o2 o3 in
          fd_set (i,j) c;
          d_set (i,j) c
        else
          let o3 = (
            let cf, mf =
              fd_get ((l1 i)-1, (l2 j)-1)
            and ct, mt = d_get (i,j) in
            (cf + ct, mt @ mf)
          )
          in
          fd_set (i,j) (cmin o1 @@ min o2 o3 )
      done
    done;
  in
  (List.iter k1 ~f:(fun x ->
      List.iter k2 ~f:(fun y ->
        populate_forest_dist_matrix x y
      )
    )
  );
  (d_get (tt1, tt2))

let zs_tree_dist_and_map t1 t2 ~eq = 
  printf "ZS Main, |T1| = %d, |T2| = %d \n" (Tree.size(t1)) (Tree.size(t2)); 
  let (Branch((tt1,_),_) as pt1), (Branch((tt2,_),_) as pt2) = Tree.convert_to_postorder t1, Tree.convert_to_postorder t2 in
  let lookup1, lookup2 = postorder_lookup pt1, postorder_lookup pt2 in
  let l1, l2 = leaf_lookup pt1, leaf_lookup pt2 in
  let k1, k2 = lr_keyroots pt1, lr_keyroots pt2 in
  let ts1, ts2 = Tree.size t1 + 1, Tree.size t2 + 1 in
  let fd = Array.create ~len:(ts1 * ts2) 0 in
  let seq d o = d + match o with
  | Some i, Some j  -> if eq (lookup1 i) (lookup2 j) then 0 else 2
  | Some _, None    -> 1
  | None,   Some _  -> 1
  | _               -> assert false
  in
  let d = Array.create ~len:(ts1 * ts2) (0, (-1,-1)) in
  let d_get (i,j) = Array.get d (i*ts2 + j) in
  let d_set (i,j) v = Array.set d (i*ts2 + j) v in
  let fd_get (i,j) = Array.get fd (i*ts2 + j) in
  let fd_set (i,j) v = Array.set fd (i*ts2 + j) v in
  let populate_forest_dist_matrix x y =
    let n1, n2 = (l1 x) - 1, (l2 y) - 1 in
    let () = fd_set (n1,n2) 0 in
    for i = l1 x to x do
      (fd_set (i, n2) (seq (fd_get (i-1, n2)) (Some(i), None)))
    done;
    for j = l2 y to y do 
      (fd_set (n1, j) (seq (fd_get (n1, j-1)) (None, Some(j))))
    done; 
    for i = l1 x to x do 
      for j = l2 y to y do 
        let o1 = seq (fd_get (i-1,j)) (Some(i), None) in
        let o2 = seq (fd_get (i,j-1)) (None, Some(i)) in
        if l1 i = l1 x && l2 j = l2 y
        then 
          let o3 = seq (fd_get (i-1,j-1)) (Some(i), Some(j)) in 
          let c = min o1 @@ min o2 o3 in
          fd_set (i,j) c;
          d_set (i,j) (c, (x,y))
        else
          let o3 = (
            let cf =
              fd_get ((l1 i)-1, (l2 j)-1)
            and ct, _ = d_get (i,j) in
            (cf + ct)
          )
          in
          fd_set (i,j) ( min o1 @@ min o2 o3 )
      done
    done;
  in
  let get_mapping () = 
    (* Gets the path to this point in fd *)
    (* Need to use cps to ensure that the array is only corrupted after it is used *)
    let rec get_mapping_fd i j x y cont =
      let n1, n2 = (l1 x) - 1, (l2 y) - 1 in
      let k_insert = fun acc -> cont (TInsert j :: acc) in
      let k_delete = fun acc -> cont (TDelete i :: acc) in
      let k_match  = fun acc -> cont (TMatch (i,j) :: acc) in
      match i,j with
      | i,j when i = n1 && j = n2 -> cont []
      | i,j when i = n1 -> 
          get_mapping_fd i (j-1) x y k_insert
      | i,j when j = n2 -> 
          get_mapping_fd (i-1) j x y k_delete
      | _ -> 
          let cmin ((i,_) as a) ((j,_) as b) = if i < j then a else b in 
          let m = 
            cmin
              (let i',j' = i-1,j in
               seq (fd_get (i',j')) (Some(i), None),
               fun () -> get_mapping_fd i' j' x y k_delete
              )
              (let i',j' = i,j-1 in
               seq (fd_get (i',j')) (None, Some(i)),
               fun () -> get_mapping_fd i' j' x y k_insert
              )
          in
          if (l1 i = l1 x && l2 j = l2 y)
          then 
            let _, f = cmin 
                m
                (let i',j' = i-1,j-1 in
                 seq (fd_get (i',j')) (Some(i), Some(j)),
                 fun () -> get_mapping_fd i' j' x y k_match
                )
            in f ()
          else 
            let _, f = cmin
              m
              (let p,q = (l1 i) - 1, (l2 j) - 1 in
               let ct, (x',y') = d_get (i,j) in
               ct + (fd_get (p,q)),
               (fun () -> get_mapping_fd p q x y (
                  fun acc -> 
                    populate_forest_dist_matrix x' y';
                    let opst = (get_mapping_fd i j x' y' (fun x -> x)) in
                    cont (opst @ acc)
                )
               )
              )
            in f ()
    in
    let _, (x,y) = d_get (tt1, tt2) in
    populate_forest_dist_matrix x y;
    get_mapping_fd tt1 tt2 x y (fun x -> x)
  in
  (List.iter k1 ~f:(fun x ->
      List.iter k2 ~f:(fun y ->
        populate_forest_dist_matrix x y
      )
    )
  );
  (fst (d_get (tt1, tt2)), get_mapping () )

(* Method: apply all deletions and relabellings, apply insertions *)
type 'a deletion = Deletion of int [@@deriving sexp]

type 'a relabel =
  | Relabel of int * 'a * 'a
  (* Index * current value * target value *)
[@@deriving sexp]

type 'a insertion_tree =
  | IBranch of 'a * 'a insertion_tree list
  | ISkip of int
[@@deriving sexp]

type 'a rooted_insertion_tree = int * 'a insertion_tree list 
[@@deriving sexp]

type 'a script =
  'a relabel list * 'a deletion list * 'a rooted_insertion_tree list
[@@deriving sexp]

type 'a match_insertion = MIMatch of int (* t1_idx *)
                        | MIInsert of int * 'a

let add_pseudo_root (Branch ((i, v), _) as tree) = Branch ((i + 1, v), [tree])

let remove_pseudo_root tree =
  match tree with Branch (_, [r]) -> r | Branch (_, _) -> assert false

let script_from_mapping mapping t1 t2 ~eq =
  let pt1, pt2 = convert_to_postorder t1, convert_to_postorder t2 in
  let insertions = 
    let convert_to_match_insert pt2 =
      Tree.tree_map pt2 ~f:(fun (j,v) ->
        match (List.find_exn mapping ~f:
                 (
                   fun x -> match x with 
                     | TInsert j' -> j' = j
                     | TMatch (_,j') -> j' = j
                     | _ -> false
                 )
              ) with
        | TInsert j' -> MIInsert(j', v)
        | TMatch (i',_) -> MIMatch(i')
        | _ -> assert false
      )
    in
    let root_idx = match pt1 with
        Branch((i,_),_) -> i
    in
    let mit2 = (Branch(MIMatch (root_idx + 1), [convert_to_match_insert pt2])) in
    let (rooted_insertion_trees, _) : 'a rooted_insertion_tree list * 'a insertion_tree = 
      Tree.fold mit2 ~f:( fun v children -> 
          let acc_ritrees = List.fold children 
              ~init:[] 
              ~f:(fun acc (ritrees, _) -> acc @ ritrees) 
          in match v with
          | MIMatch(i) -> (
              (if List.exists children ~f:(fun (_,itree) -> 
                   match itree with
                   | IBranch(_,_) -> true
                   | ISkip(_) -> false
                 ) then
                 (
                   (i, List.map children 
                      ~f:(fun (_, (itree:'a insertion_tree)) -> itree)
                   )::acc_ritrees
                 )
               else acc_ritrees
              ), ISkip(i))
          | MIInsert(_, v) -> (
              acc_ritrees,
              IBranch(v, List.fold_right children ~init:[] 
                        ~f:(
                          fun (_, itree) acc -> 
                            itree :: acc
                        )
                     )
            )
        ) in
    rooted_insertion_trees
  in
  let deletions =
    List.fold mapping ~init:[] ~f:(fun ds x ->
        match x with TDelete v -> Deletion v :: ds | _ -> ds )
  in
  let relabels =
    let t1arr (idx : int) =
      List.Assoc.find_exn ~equal:( = ) (postordering_label_list pt1) idx
    in
    let t2arr (idx : int) =
      List.Assoc.find_exn ~equal:( = ) (postordering_label_list pt2) idx
    in
    List.fold mapping ~init:[] ~f:(fun rs x ->
        match x with
        | TMatch (i, j) ->
            let v1, v2 = (t1arr i, t2arr j) in
            if eq v1 v2 then rs else Relabel (i, v1, v2) :: rs
        | _ -> rs )
  in
  ((relabels, deletions, insertions) : 'a script)

let diff : 'a tree -> 'a tree -> eq:('a -> 'a -> bool) -> 'a script =
 fun t1 t2 ~eq ->
  let _, map = zs_tree_dist_and_map t1 t2 ~eq in
  script_from_mapping map t1 t2 ~eq

(*---- Apply patch ---------------------------------*)
let apply_relabels : 'a relabel list -> 'a pi_tree -> 'a pi_tree =
 fun rs tree ->
  Tree.tree_map tree ~f:(fun (i, v) ->
      match List.find rs ~f:(fun (Relabel (i', _, _)) -> i = i') with
      | Some (Relabel (_, _, v')) -> (i, v')
      | None -> (i, v) )

let apply_deletions ds (tree : 'a pi_tree) =
  (* iter takes a branch, if it is to be deleted then returns its children (in deleted form) *)
  let rec iter tree =
    match tree with Branch ((i, v), cs) ->
      let cs' =
        List.fold_right cs ~init:[] ~f:(fun c acc ->
            (* c' is the deleted form of c i.e. a list *)
            let c' = iter c in
            c' @ acc )
      in
      if List.exists ds ~f:(fun (Deletion j) -> i = j) then cs'
      else [Branch ((i, v), cs')]
  in
  match iter tree with
  | [x] -> x
  | [] -> assert false (* all nodes deleted => can't happen with superroot *)
  | _ :: _ :: _ -> assert false (* root deleted => can't happen with superroot *)

let apply_insertions (its : 'a rooted_insertion_tree list) (tree : 'a pi_tree)
    =
  let equal_index (Branch ((i, _), _)) j = i = j in
  (* Since both trees have same startting point can walk both and perform insertions *)
  let rec apply_insert_tree insertion_tree possible_skips =
    match (insertion_tree, possible_skips) with
    | IBranch (v, ics), ps ->
        let cs, ps' =
          List.fold_left ics ~init:([], ps) ~f:(fun (cs', ps') ic ->
              let c'', ps'' = apply_insert_tree ic ps' in
              (c'' :: cs', ps'') )
        in
        (Branch ((-1, v), List.rev cs), ps')
    | ISkip i, s :: ps ->
        assert (equal_index s i) ;
        (s, ps)
    | ISkip _, [] ->
        raise (Invalid_argument "More skips than children of node")
  in
  let rec iter (Branch ((i, v), cs)) =
    let cs' = List.map cs ~f:(fun c -> iter c) in
    (* Check if an insertion tree starts here*)
    match List.find its ~f:(fun (parent_idx, _) -> parent_idx = i) with
    | Some (_, its) ->
        let rcs'', ps =
          List.fold_left its ~init:([], cs') ~f:(fun (cs'', ps) it ->
              let c', ps' = apply_insert_tree it ps in
              (c' :: cs'', ps') )
        in
        Branch ((i, v), List.rev rcs'' @ ps)
    | None -> Branch ((i, v), cs')
  in
  iter tree

let apply_diff (rs, ds, is) t1 =
  t1 
  |> convert_to_postorder 
    |> add_pseudo_root 
      |> apply_relabels rs
      |> apply_deletions ds 
      |> apply_insertions is 
    |> remove_pseudo_root
  |> convert_from_postorder
