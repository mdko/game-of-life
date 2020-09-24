open Lib
open Boards

let board = Alcotest.testable Types.pp_board ( = )

let still_test raw_board =
  Alcotest.(check board) "cells after a step"
    (Board.array_to_board raw_board |> Board.next_board)
    (Board.array_to_board raw_board) 

let test_block () = still_test Still_lifes.block
let test_beehive () = still_test Still_lifes.beehive
let test_loaf () = still_test Still_lifes.loaf
let test_boat () = still_test Still_lifes.boat
let test_tub () = still_test Still_lifes.tub

let step_n steps board =
  let rec aux step board = 
    if step = (steps)
    then board
    else Board.next_board board |> aux (step + 1)
  in aux 0 board

let test_periodic raw_board expected_raw_board steps =
  Alcotest.(check board) ("board after " ^ string_of_int steps ^ " steps")
    (Board.array_to_board raw_board |> step_n steps)
    (Board.array_to_board expected_raw_board)

let test_blinker () =
  test_periodic Oscillators.blinker Oscillators.blinker_after_1_step 1;
  test_periodic Oscillators.blinker Oscillators.blinker 2

let test_toad () =
  test_periodic Oscillators.toad Oscillators.toad_after_1_step 1;
  test_periodic Oscillators.toad Oscillators.toad 2

let test_beacon () =
  test_periodic Oscillators.beacon Oscillators.beacon_after_1_step 1;
  test_periodic Oscillators.beacon Oscillators.beacon 2

let test_pulsar () =
  test_periodic Oscillators.pulsar Oscillators.pulsar_after_1_step 1;
  test_periodic Oscillators.pulsar Oscillators.pulsar_after_2_steps 2;
  test_periodic Oscillators.pulsar Oscillators.pulsar 3

let test_glider () =
  test_periodic Spaceships.glider Spaceships.glider_after_1_step 1;
  test_periodic Spaceships.glider Spaceships.glider_after_2_steps 2;
  test_periodic Spaceships.glider Spaceships.glider_after_3_steps 3

let test_lwss () =
  test_periodic Spaceships.lwss Spaceships.lwss_after_1_step 1;
  test_periodic Spaceships.lwss Spaceships.lwss_after_2_steps 2;
  test_periodic Spaceships.lwss Spaceships.lwss_after_3_steps 3

let () =
  let open Alcotest in
  run "MyTests" [
    "still_lifes", [
      test_case "block" `Quick test_block;
      test_case "beehive" `Quick test_beehive;
      test_case "loaf" `Quick test_loaf;
      test_case "boat" `Quick test_boat;
      test_case "tub" `Quick test_tub;
    ];
    "oscillators", [
      (* period 2 *)
      test_case "blinker" `Quick test_blinker;
      test_case "toad" `Quick test_toad;
      test_case "beacon" `Quick test_beacon;
      (* period 3 *)
      test_case "pulsar" `Quick test_pulsar;
    ];
    "spaceships", [
      test_case "lwss" `Quick test_lwss;
    ];
  ]