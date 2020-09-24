open Lib
open Core

(* TODO
   - add controls to go back/forward steps
   - [x] expand frontier of grid as needed
   - contract frontier of grid as needed (i.e. probably when there are two empty side-by-side rows/cols, reduce to one)
     - possibly track and update, but not display, out-of-bounds living cells, in case they eventually
       feed back into the main display
     - also be able to resize the terminal and have this reflected
   - add ability to start with a blank 100x100 grid, select cells with mouse, and start simulation
 *)

let seed_lookup (seed_name: string) : (int * int * int list) =
  match seed_name with
  | "beehive" -> Seeds.Still_lifes.beehive
  | "block" -> Seeds.Still_lifes.block
  | "boat" -> Seeds.Still_lifes.boat
  | "tub" -> Seeds.Still_lifes.tub
  | "beacon" -> Seeds.Oscillators.beacon
  | "blinker" -> Seeds.Oscillators.blinker
  | "pulsar" -> Seeds.Oscillators.pulsar
  | "toad" -> Seeds.Oscillators.toad
  | "one_zero_one" -> Seeds.Oscillators.one_zero_one
  | "glider" -> Seeds.Spaceships.glider
  | "two_glider_mess" -> Seeds.Spaceships.two_glider_mess
  | "gosper_glider_gun" -> Seeds.Spaceships.gosper_glider_gun
  | "lwss" -> Seeds.Spaceships.lwss
  | "mwss" -> Seeds.Spaceships.mwss
  | "hwss" -> Seeds.Spaceships.hwss
  | _ -> failwith "bad seed name"

let command =
  Command.basic
    ~summary:"Simulate Conway's Game of Life"
    Command.Let_syntax.(
    Command.Param.(
      let%map 
        seed = anon ("seed" %: string)
        and ticks = flag "--ticks" ~doc: "number of ticks to simulate (default to infinity)" (optional int)
        and delay = flag "--delay" ~doc: "seconds between each tick" (optional_with_default 0.5 float)
        and tty = flag "--tty" no_arg ~doc:" produce output in the terminal"
      in
        let board = seed_lookup seed |> Board.array_to_board in
        let () = match ticks with
          | Some ticks -> if ticks < 0 then failwith "invalid ticks (must be > 0)"
          | None -> ()
        in
        match tty with
        | true -> (fun () -> Tty.simulate board ~delay ~ticks)
        | false -> (fun () -> Gui.simulate board delay ticks)))

let () =
    Command.run ~version:"1.0" command
