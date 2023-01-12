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


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)

  (* These command-line arguments are not used for the moment. *)

  in

  (* Open file *)
  let (graph, src, fin, list_guest, list_host) = from_file infile in

  let (final_graph, flow) = ff graph src fin in  

  (* Rewrite the graph that has been read. *)
  let () = out_stream outfile final_graph list_guest list_host in 
  let () = export "graph" (gmap final_graph string_of_int) fin in

  Sys.command "dot -Tsvg graph > out/final_graph.svg";

  ()

