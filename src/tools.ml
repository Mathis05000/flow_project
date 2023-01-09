(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let rec clone_nodes gr = n_fold gr (fun x id -> new_node x id) empty_graph 
;;

let gmap gr f = let gr2 = clone_nodes gr in
e_fold gr (fun x id1 id2 v -> new_arc x id1 id2 (f v)) gr2 
;;

let add_arc gr id1 id2 v = match (find_arc gr id1 id2) with
  |None ->  new_arc gr id1 id2 v
  |Some x -> new_arc gr id1 id2 (x + v)
;;

let sub_arc gr id1 id2 v = match (find_arc gr id1 id2) with
  |None ->  new_arc gr id1 id2 v
  |Some x -> new_arc gr id1 id2 (x - v)
;;

