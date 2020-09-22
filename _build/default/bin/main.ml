open Lib

let () =
    let board = Board.array_to_board Boards.Still_lifes.block in
    print_endline (Board.board_to_string board);
    let board = Board.next_board board in
    print_endline (Board.board_to_string board)