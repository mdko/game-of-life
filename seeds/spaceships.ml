let glider = (
  6,
  5,
  [0;0;0;0;0;
   0;0;1;0;0;
   0;0;0;1;0;
   0;1;1;1;0;
   0;0;0;0;0;
   0;0;0;0;0]
)

let glider_after_1_step = (
  6,
  5,
  [0;0;0;0;0;
   0;0;0;0;0;
   0;1;0;1;0;
   0;0;1;1;0;
   0;0;1;0;0;
   0;0;0;0;0]
)

let glider_after_2_steps = (
  6,
  5,
  [0;0;0;0;0;
   0;0;0;0;0;
   0;0;0;1;0;
   0;1;0;1;0;
   0;0;1;1;0;
   0;0;0;0;0]
)

let glider_after_3_steps = (
  6,
  5,
  [0;0;0;0;0;
   0;0;0;0;0;
   0;0;1;0;0;
   0;0;0;1;1;
   0;0;1;1;0;
   0;0;0;0;0]
)

(* period of 4, moves *)
let lwss = (
  7,
  8,
  [0;0;0;0;0;0;0;0;
   0;1;0;0;1;0;0;0;
   0;0;0;0;0;1;0;0;
   0;1;0;0;0;1;0;0;
   0;0;1;1;1;1;0;0;
   0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;
  ]
)

let lwss_after_1_step = (
  7,
  8,
  [0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;
   0;0;0;0;1;1;0;0;
   0;0;1;1;0;1;1;0;
   0;0;1;1;1;1;0;0;
   0;0;0;1;1;0;0;0;
   0;0;0;0;0;0;0;0;
  ]
)

let lwss_after_2_steps = (
  7,
  8,
  [0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;
   0;0;0;1;1;1;1;0;
   0;0;1;0;0;0;1;0;
   0;0;0;0;0;0;1;0;
   0;0;1;0;0;1;0;0;
   0;0;0;0;0;0;0;0;
  ]
)

let lwss_after_3_steps = (
  7,
  8,
  [0;0;0;0;0;0;0;0;
   0;0;0;0;1;1;0;0;
   0;0;0;1;1;1;1;0;
   0;0;0;1;1;0;1;1;
   0;0;0;0;0;1;1;0;
   0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;
  ]
)

let mwss = (
  9,
  10,
  [0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;
   0;0;0;1;1;1;1;1;0;0;
   0;0;1;0;0;0;0;1;0;0;
   0;0;0;0;0;0;0;1;0;0;
   0;0;1;0;0;0;1;0;0;0;
   0;0;0;0;1;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;
  ]
)

let hwss = (
  9,
  11,
  [0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;1;1;1;1;1;1;0;0;
   0;0;1;0;0;0;0;0;1;0;0;
   0;0;0;0;0;0;0;0;1;0;0;
   0;0;1;0;0;0;0;1;0;0;0;
   0;0;0;0;1;1;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;
  ]
)

let two_glider_mess = (
  6,
  12,
  [0;0;1;0;0;0;0;0;0;0;0;0;
   1;0;1;0;0;0;0;0;0;0;0;0;
   0;1;1;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;1;
   0;0;0;0;0;0;0;0;0;1;1;0;
   0;0;0;0;0;0;0;0;0;0;1;1;
  ]
)

let gosper_glider_gun = (
  9,
  36,
  [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;0;1;0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;0;0;0;0;0;1;1;0;0;0;0;0;0;0;0;0;0;0;0;1;1;
   0;0;0;0;0;0;0;0;0;0;0;1;0;0;0;1;0;0;0;0;1;1;0;0;0;0;0;0;0;0;0;0;0;0;1;1;
   1;1;0;0;0;0;0;0;0;0;1;0;0;0;0;0;1;0;0;0;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
   1;1;0;0;0;0;0;0;0;0;1;0;0;0;1;0;1;1;0;0;0;0;1;0;1;0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;1;0;0;0;0;0;1;0;0;0;0;0;0;0;1;0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;1;0;0;0;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
   0;0;0;0;0;0;0;0;0;0;0;0;1;1;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
  ]
)
