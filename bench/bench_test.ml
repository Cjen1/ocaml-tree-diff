open Lib
open Utils
open Core

(*let tester_time f : float = 
  let st = Unix.gettimeofday () in
  let _ = f () in
  let ft = Unix.gettimeofday () in
  (ft -. st)

let tester_space f = 
  let max_size = ref 0 in
  let alarm = Gc.Expert.Alarm.create (fun () -> 
    let live = (Gc.stat ()).live_words in
    max_size := max (!max_size) (live)
    )
  in
  let sm = Gc.allocated_bytes () in
  let _ = f () in
  let sf = Gc.allocated_bytes () in
  Gc.full_major ();
  Gc.Expert.Alarm.delete alarm;
  (sf -. sm, !max_size)
*)

let wrap f t1 t2 ~eq = 
  ignore(f t1 t2 ~eq)


let algo_vers = [ 
  ("0-no_map",wrap Diff.zs_tree_dist0);
  ("1-no_map",wrap Diff.zs_tree_dist1);
  ("1- map",wrap Diff.zs_tree_dist_and_map1);
]

let test_sizes = [0;20;40;80;160;320;480]

let gen_trees size = 
  let r = Tree.Branch('r', []) in
  let t1 = Gentree.gen_size r size in
  let t2 = Gentree.gen_tree t1 in
  (t1, t2)

module CLI = Minicli.CLI
let main () = 
        let argc, args = CLI.init () in
        if argc = 1 then
                (printf "ERROR")
        else
        let size = (CLI.get_int ["-s";"--size"] args) in
        let ver = CLI.get_int ["-v";"--version"] args in
        let name, func = List.nth_exn algo_vers ver in
        let t1, t2 = gen_trees size in
        printf "%s = \n %s \n" name (func t1 t2 ~eq:Char.equal |> [%sexp_of: unit] |> Sexp.to_string_hum )

let () = main ()


