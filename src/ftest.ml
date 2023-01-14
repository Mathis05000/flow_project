open Gfile_new
open Tools
open Algo

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf "\nUsage: %s infile outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)

  in

  (* Open file and generate graph and dictionaries associated with the input file *)
  let (graph, src, fin, list_guest, list_host) = from_file infile in

  (* Execute Ford Fulkerson algorithm for generate the final graph *)
  let (final_graph, flow) = ff graph src fin in  

  (* generate output file with the result of the distribution of guests with hosts *)
  let () = out_stream outfile final_graph list_guest list_host in 

  (* generate the final graph associated with the result of the problem with Graphviz library *)
  let () = export "graph" (gmap final_graph string_of_int) fin in
  Sys.command "dot -Tsvg graph > out/final_graph.svg";

  ()

