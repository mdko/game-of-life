open Core
open Types

let simulate board ~delay ~ticks =
  let win = Curses.initscr () in
  let _err = Curses.curs_set 0 in (* make cursor invisible *)
  let _err = Curses.start_color () in
  let acs = Curses.get_acs_codes () in
  let rec aux board tick =
    let _err = Curses.move 0 0 in
    Board.rows_of_board board |> List.iter ~f:(
      fun cells ->
        List.iter cells ~f:(fun cell ->
          (match cell.state with
          | Alive -> let _err = Curses.waddch win acs.ckboard in ()
          | Dead -> let _err = Curses.waddstr win " " in ()
          );
        );
        let _err = Curses.waddstr win "\n" in ()
    );
    let _err = Curses.refresh () in
    if Option.is_some ticks && tick = (Option.value_exn ticks - 1)
      then ()
      else (
        let _ = Unix.nanosleep delay in
        aux (Board.next_board board) (tick + 1)
      )
  in aux board 0;
  Curses.endwin ()
