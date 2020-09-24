open Lib
open Core

(* TODO
   - add controls to go back/forward steps
   - [x] expand frontier of grid as needed
   - contract frontier of grid as needed (i.e. probably when there are two empty side-by-side rows/cols, reduce to one)
 *)

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
  | "glider" -> Boards.Spaceships.glider
  | "lwss" -> Boards.Spaceships.lwss
  | "mwss" -> Boards.Spaceships.mwss
  | "hwss" -> Boards.Spaceships.hwss
  | _ -> failwith "bad board name"

let command =
  Command.basic
    ~summary:"Simulate Conway's Game of Life for a given starting board"
    Command.Let_syntax.(
    Command.Param.(
      let%map 
        board = anon ("board" %: string)
        and steps = anon (maybe ("steps" %: int))
        and tty = flag "--tty" no_arg ~doc:" produce output in the terminal"
      in
        let board = board_lookup board |> Board.array_to_board in
        let () = match steps with
          | Some steps -> if steps < 0 then failwith "invalid steps (must be > 0)"
          | None -> ()
        in
        match tty with
        | true -> (fun () -> Tty.simulate board steps)
        | false -> (fun () -> Gui.simulate board steps)))

let () =
    Command.run ~version:"1.0" command