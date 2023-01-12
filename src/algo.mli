open Graph
open Tools

val find_path: int graph -> id -> id -> int list

val min_arc_path: int graph -> id list -> int 

val iter_FF: int graph -> id list -> int -> int graph 

val ff: int graph -> id -> id -> int graph * int