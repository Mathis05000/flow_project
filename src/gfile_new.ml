open Graph
open Printf
    
type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used), and its id.
   n 88.8 209.7 0
   n 408.9 183.0 1

   % Edges: e source dest label id  (the edge id is not used).
   e 3 1 11 0 
   e 0 2 8 1

*)

(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count id1 id2 lbl -> fprintf ff "e %d %d %d %s\n" id1 id2 count lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()

let export path graph fin =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "digraph finite_state_machine {\n" ;
  fprintf ff "fontname=\"Helvetica,Arial,sans-serif\"\nnode [fontname=\"Helvetica,Arial,sans-serif\"]\nedge [fontname=\"Helvetica,Arial,sans-serif\"]\nrankdir=LR;\nnode [shape = doublecircle]; 0 %d ;\nnode [shape = circle];\n" fin;  
  (* Write all nodes and arcs in Graphviz format *)
  e_iter graph (fun id1 id2 label -> fprintf ff "%.1d -> %.1d [label = \"%s\"];\n" id1 id2 label);
  fprintf ff "\n" ;

  fprintf ff "}\n" ;
  
  close_out ff ;
  ()
  
(* Générate result file for users *)

(* 
X dors chez Y 
Z dors chez Y
...            
*)

let out_stream path graph list_guest list_host = 
  let ff = open_out path in
  let rec loop list_guest = match list_guest with
  |[] -> close_out ff; ()
  |(id, name_guest, _)::rest_guest -> 
    let arc = out_arcs graph id in
    let rec loop2 arc = match arc with
      |[] -> fprintf ff "%s n'a pas de place pour dormir\n" name_guest
      |(id_host, lbl)::rest -> if lbl = 0 then List.iter (fun (id, name_host, _, _) -> if id = id_host then fprintf ff "%s dors chez %s\n" name_guest name_host) list_host else loop2 rest
    in
    loop2 arc;
    loop rest_guest
  in

  loop list_guest




(* Reads a line with a host and update dictionary between node's id and host. *)
let read_host line id list_host =
  try Scanf.sscanf line "h %s , %d %[^\n]" (fun name nb_place list_c -> Printf.printf "%s\n" list_c; List.append list_host [(id, name, nb_place, (String.split_on_char ' ' list_c))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with a guest and update dictionary between node's id and guest. *)
let read_guest line id list_guest = 
  try Scanf.sscanf line "g %s , %[^\n]" (fun name list_c -> List.append list_guest [(id, name, (String.split_on_char ' ' list_c))])
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a comment or fail. *)
let read_comment graph line list_guest list_host =
  try Scanf.sscanf line " %%" (graph, list_guest, list_host)
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"


(* Generate graph and dictionaries associated with input file *)
let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph id list_guest list_host =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then (graph, list_guest, list_host)

        (* The first character of a line determines its content : h or g *)
        else match line.[0] with
          | 'h' -> ((new_node graph id), list_guest, (read_host line id list_host))
          | 'g' -> ((new_node graph id), (read_guest line id list_guest), list_host)

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line list_guest list_host
      in    
      let (graph, list_guest, list_host) = graph2 in
      loop graph (id + 1) list_guest list_host

    with End_of_file -> (graph, id, list_guest, list_host) (* Done *)

  in

  let (tmp_graph, id_fin, list_guest, list_host) = loop empty_graph 0 [] [] in
  
  close_in infile ;

  (* add source node*)
  let tmp_graph = new_node tmp_graph 0 in

  (* add end node*)
  let tmp_graph = new_node tmp_graph id_fin in

  (* Generate arcs *)
  let init_arc graph list_guest list_host = 

    (* Generate arcs from source to guest with capacity of 1 *)
    let rec loop graph list_guest = match list_guest with
      |[] -> graph
      |(id, _, _)::rest -> loop (new_arc graph 0 id 1) rest
    in
    
    let tmp_graph = loop graph list_guest in

    (* Generate arcs from hosts to end node with capacity of hosts capacities *)
    let rec loop graph list_host = match list_host with
      |[] -> graph
      |(id, _, nb_place, _)::rest -> loop (new_arc graph id id_fin nb_place) rest
    in

    let tmp_graph = loop tmp_graph list_host in

    (* Generate arcs from guests to hosts if guests can stay with host *)
    let rec loop graph list_host = match list_host with
      |[] -> graph
      |(id_host, _, _, list_c_h)::rest_host -> 

        (let rec loop2 graph list_c_h list_guest = match list_guest with
          |[] -> graph
          |(id_guest, _, list_c_g)::rest_guest -> 

            (let rec loop3 list_c_g list_c_h = match list_c_g with
              |[] -> true
              |x::rest -> Printf.printf "size : %d %d" (List.length list_c_g) (List.length list_c_h); if (List.mem x list_c_h) then false else loop3 rest list_c_h
            in
            if (loop3 list_c_g list_c_h) then loop2 (new_arc graph id_guest id_host 1) list_c_h rest_guest else loop2 graph list_c_h rest_guest)
          
        in
        loop (loop2 graph list_c_h list_guest) rest_host)

    in
    loop tmp_graph list_host
  in
  
  let tmp_graph = init_arc tmp_graph list_guest list_host in

  (tmp_graph, 0, id_fin, list_guest, list_host)

