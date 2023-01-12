open Graph
open Tools

let find_path g id1 id2 = 

  let rec loop acu r = match r with
    |[] -> []
    |(id,lbl) :: rest when id = id2 && lbl != 0 -> id2 :: acu 
    |(idx,lblx) :: rest -> if (List.mem idx acu) || (lblx = 0)
      then loop acu rest 
      else match (loop (idx::acu) (out_arcs g idx)) with
        |[] -> loop acu rest
        |x -> x
  in

  let rslt = loop [id1] (out_arcs g id1) in
  match (List.rev rslt) with
    |[id] when id = id1 -> []
    |x -> x
;;

let min_arc_path g path = 
  let rec loop path acu = match path with
  |[a] -> acu
  |x::y::rest -> let tmp = begin match (find_arc g x y) with
      |Some x -> x
      |_ -> assert false
      end
    in 
      if tmp < acu then loop (y::rest) tmp else loop (y::rest) acu
  |[] -> assert false
  in
 
  loop path max_int
  ;;



let iter_FF gr path add = 
  let rec loop path gr_acu = match path with
  |[x] -> gr_acu
  |x::y::rest -> let tmp = sub_arc gr_acu x y add 
    in
    loop (y::rest) (add_arc tmp y x add)
  |[] -> assert false
  in

  loop path gr
;;


let ff gr id1 id2 = 
  let rec loop path gr_acu flow = match path with
    |[] -> (gr_acu, flow)
    |x -> let tmp_flow = (min_arc_path gr_acu x) in
      let tmp_gr = iter_FF gr_acu x tmp_flow in
      (*Printf.printf "loop : %d\n%!" tmp_flow;*)
      loop (find_path tmp_gr id1 id2) tmp_gr (flow + tmp_flow) 
  in

  loop (find_path gr id1 id2) gr 0
;;

