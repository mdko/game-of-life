open Lib
open Core

let board_lookup (board_name: string) : (int * int * int list) =
  match board_name with
  | "beehive" -> Boards.Still_lifes.beehive
  | "block" -> Boards.Still_lifes.block
  | "boat" -> Boards.Still_lifes.boat
  | "tub" -> Boards.Still_lifes.tub
  | "beacon" -> Boards.Oscillators.beacon
  | "blinker" -> Boards.Oscillators.blinker
  | "pulsar" -> Boards.Oscillators.pulsar
  | "toad" -> Boards.Oscillators.toad
  | _ -> failwith "bad board name"

let simulate board steps =
  let rec aux board step =
    Board.board_to_string board |> print_endline;
    if step = (steps - 1)
      then ()
      else (
        Unix.sleep 1;
        aux (Board.next_board board) (step + 1)
      )
  in aux board 0

let command =
  Command.basic
    ~summary:"Simulate Conway's Game of Life for a given starting board"
    Command.Param.(
      map (both
        (anon ("board" %: string))
        (anon ("steps" %: int)))
      ~f:(fun (board, steps) ->
          let board = board_lookup board |> Board.array_to_board in
          let () = if steps < 0 then failwith "invalid steps (must be > 0)" in
          (fun () -> simulate board steps)))

let () =
    Command.run ~version:"1.0" command