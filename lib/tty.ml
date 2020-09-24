open Core
open Types

let simulate board steps =
  let win = Curses.initscr () in
  let _err = Curses.curs_set 0 in (* make cursor invisible *)
  let _err = Curses.start_color () in
  let acs = Curses.get_acs_codes () in
  (* let alive_color = 1 in
  let dead_color = 2 in
  let _err = Curses.init_pair alive_color Curses.Color.white Curses.Color.black in
  let _err = Curses.init_pair dead_color Curses.Color.black Curses.Color.white in *)
  let rec aux board step =
    (* Board.board_to_string board |> Curses.mvwaddstr win 0 0 |> fun _ -> (); *)
    let _err = Curses.move 0 0 in
    Board.board_to_array board |> List.iter ~f:(
      fun cells ->
        List.iter cells ~f:(fun cell ->
          (match cell.state with
          (* | Alive -> Curses.wbkgd win alive_color; let _err = Curses.waddstr win "1" in ()
          | Dead -> Curses.wbkgd win dead_color; let _err = Curses.waddstr win "0" in () *)
          | Alive -> let _err = Curses.waddch win acs.ckboard in ()
          | Dead -> let _err = Curses.waddstr win " " in ()
          );
        );
        let _err = Curses.waddstr win "\n" in ()
    );
    let _err = Curses.refresh () in
    if Option.is_some steps && step = (Option.value_exn steps - 1)
      then ()
      else (
        Unix.sleep 1;
        aux (Board.next_board board) (step + 1)
      )
  in aux board 0;
  Curses.endwin ()