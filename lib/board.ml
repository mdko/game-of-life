open Types
open Core

(* let%test _ = make_pos 3 0 = {row = 0; col = 0}
let%test _ = make_pos 3 8 = {row = 2; col = 2}
let%test _ = make_pos 3 9 = {row = 3; col = 0} *)

let is_bad_pos {nrows; ncols; _} {row; col} : bool =
  row < 0 || row >= nrows || col < 0 || col >= ncols

let get_cell board pos =
  if is_bad_pos board pos
    then None
    else
      let {cells; ncols; _} = board in
      let {row; col} = pos in
      Some (List.nth_exn cells (row * ncols + col))

let set_cell board cell =
  let cells = List.fold_right ~f:(fun curr accum ->
    if curr.position = cell.position 
    then
      cell :: accum
    else
      curr :: accum
    )
  ~init:[]
  board.cells
  in
  {board with cells}

let rel_pos {row; col} = function
  | `upleft -> {row = row-1; col=col-1}
  | `up -> {row = row-1; col}
  | `upright -> {row = row-1; col=col+1}
  | `left -> {row; col=col-1}
  | `right -> {row; col=col+1}
  | `downleft -> {row = row + 1; col=col-1}
  | `down -> {row = row + 1; col}
  | `downright -> {row = row + 1; col=col+1}

let map_opt (f: 'a -> 'b option) (l: 'a list) : 'b list =
  List.fold_right 
  ~f:(fun el accum -> 
    match f el with
    | None -> accum
    | Some el -> el :: accum
  )
  ~init:[]
  l

let get_neighbors board position : cell list =
  map_opt (fun dir -> rel_pos position dir |> get_cell board) [
    `upleft;
    `up;
    `upright;
    `left;
    `right;
    `downleft;
    `down;
    `downright;
  ]

let update board cell =
  let n_live_neighbors = get_neighbors board cell.position |> List.filter ~f:(fun cell -> cell.state = Alive) |> List.length in
  match cell.state with
  | Alive -> (
    match n_live_neighbors with
    | 0 | 1 -> {cell with state = Dead}
    | 2 | 3 -> cell
    | _ -> {cell with state = Dead}
  )
  | Dead -> (
    match n_live_neighbors with
    | 3 -> {cell with state = Alive}
    | _ -> cell
  )

let insert_column board _before_col = board
let insert_row board _before_row = board

let frontier board side =
  let {nrows; ncols; _} = board in
  match side with
  | `left -> List.init nrows ~f:(fun row -> Option.value_exn (get_cell board {row; col = 0}))
  | `right -> List.init nrows ~f:(fun row -> Option.value_exn (get_cell board {row; col = ncols - 1}))
  | `top -> List.init ncols ~f:(fun col -> Option.value_exn (get_cell board {col; row = 0}))
  | `bottom -> List.init ncols ~f:(fun col -> Option.value_exn (get_cell board {col; row = nrows - 1}))

let expand_frontiers board =
  let has_living cells = List.exists cells ~f:(fun cell -> cell.state = Alive) in
  let board =
    if frontier board `left |> has_living
    then insert_column board 0
    else board
  in let board =
    if frontier board `right |> has_living
    then insert_column board (board.ncols)
    else board
  in let board = 
    if frontier board `top |> has_living
    then insert_row board 0
    else board
  in let board = if frontier board `bottom |> has_living
    then insert_row board (board.nrows)
    else board
  in board

let next_board board =
  (* Need to get list of
     - frontier cells that died; if all the cells on the frontier are dead, reduce frontier size down (unless at minimum)
     - cells past the frontier that need to be living; if any, expand frontier
  *)
  let cells = List.map ~f:(update board) board.cells in
  {board with cells}

let array_to_board = function
| (_, _, []) -> failwith "empty array"
| (nrows, ncols, xs) ->
  let make_pos ncols i =
    {row=(i / ncols); col=(i mod ncols)} in
  let cells = List.mapi ~f:(fun i n -> 
      {position = make_pos ncols i; state = (if (n = 1) then Alive else Dead)}) xs in
  {
      nrows;
      ncols;
      cells;
  } |> expand_frontiers

let board_to_array board : cell list list =
  let {cells; _} = board in
  List.fold_left ~init:[] ~f:(fun accum (c: cell) ->
    match accum with
    | [] -> [[c]]
    | (h::tl) -> (
      if c.position.col = 0
      then [c] :: h :: tl
      else (c::h) :: tl
    )
  ) cells
  |> List.rev_map ~f:(List.rev)

let board_to_string ?(f: (cell -> string) option) board : string =
  board_to_array board |>
  List.map ~f:(fun cells ->
    List.map cells ~f:(fun cell -> 
      match f with
        | None -> 
          (match cell.state with
          | Alive -> "1"
          | Dead -> "0"
          )
        | Some f -> f cell
      )
  ) |> 
  List.map ~f:(String.concat ~sep:" ") |>
  String.concat ~sep:"\n"