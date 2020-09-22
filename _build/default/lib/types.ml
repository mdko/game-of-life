type state = Dead | Alive [@@deriving eq, show]

type position = {
  row: int;
  col: int;
} [@@deriving eq, show]

type cell = {
  state: state;
  position: position;
} [@@deriving eq, show]

type board = {
  cells: cell list;
  nrows: int;
  ncols: int;
} [@@deriving eq, show]
