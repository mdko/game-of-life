open Types

val board_to_array : board -> cell list list
val board_to_string : ?f:(cell -> string) -> board -> string
val array_to_board : (int * int * int list) -> board

val get_cell : board -> position -> cell option
val set_cell : board -> cell -> board
val is_bad_pos : board -> position -> bool
val get_neighbors : board -> position -> cell list
val next_board : board -> board