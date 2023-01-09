open Gfile
open Tools
open Algo

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let int_graph = gmap graph int_of_string in
  let path = find_path int_graph _source _sink in

  let rec print t = match t with
    |[] -> ()
    |x::rest -> Printf.printf "%d " x ; print rest
  in

  (*let min = min_arc_path int_graph path in
  let gr2 = iter_FF int_graph path min in
  let gr3 = gmap gr2 string_of_int in 

  let min = min_arc_path int_graph path in
  let gr2 = iter_FF int_graph path min in
  let gr3 = gmap gr2 string_of_int in *)
  let flow = ff int_graph _source _sink in

  Printf.printf "%d" flow;




  

  (* Rewrite the graph that has been read. *)
  let () = export outfile graph in 

  ()

