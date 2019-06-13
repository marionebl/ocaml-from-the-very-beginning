type stats = { 
  lines: int; 
  chars: int; 
  words: int; 
  sents: int;
  histogram: int array;
}

val lines : stats -> int
val characters: stats -> int
val words : stats -> int
val sentences : stats -> int

val stats_from_stream : char Stream.t -> stats
val stats_from_file : string -> stats
val format_stats : stats -> string