open Core

let simulate board steps =
  let win = Curses.initscr () in
  let rec aux board step =
    Board.board_to_string board |> Curses.mvwaddstr win 0 0 |> fun _ -> ();
    (* Board.board_to_array board |> List.iter ~f:(
      fun cells ->
        let line = String.concat ~sep:" " cells in
        let _err = Curses.waddstr win line in
        let _err = Curses.waddstr win "\n" in
        ()
    ); *)
    let _err = Curses.refresh () in
    if step = (steps - 1)
      then ()
      else (
        Unix.sleep 1;
        aux (Board.next_board board) (step + 1)
      )
  in aux board 0;
  Curses.endwin ()